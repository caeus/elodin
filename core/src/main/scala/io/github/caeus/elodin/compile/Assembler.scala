package io.github.caeus.elodin.compile

import com.typesafe.scalalogging.LazyLogging
import io.github.caeus.elodin.compile.Ctx.BaseCtx
import io.github.caeus.elodin.core.{Archive, ThunkRef, Val}
import zio.{IO, Ref, Task, ZIO}

import scala.annotation.tailrec

trait Assembler {
  def assemble(name: String, node: Node): IO[CompileError, Draft]
}

final class DefaultAssembler(deps: Archive) extends Assembler with LazyLogging {
  def assemble(book: String, node: Node): IO[CompileError, Draft] = {
    new AssemblerVisitor(deps, book, node).assemble
  }
}
case class NodeBundle(
    id: String,
    ctx: Ctx,
    capturedParamsSize: Int,
    header: Seq[DeclParam],
    page: ThunkRef
)
final class AssemblerVisitor(archive: Archive, book: String, root: Node) extends LazyLogging {
  def walkDown(
      ctx: Ctx,
      trace: Set[Vector[String]],
      emitted: Ref[Map[Ctx, Set[DeclParam]]]
  ): IO[CompileError, Set[DeclParam]] = {
    def emit(ctx: Ctx)(capturedParams: Set[DeclParam]) = {
      emitted
        .update { map =>
          map.updated(ctx, capturedParams)
        }
        .map(_ => capturedParams)
    }
    if (trace contains ctx.path) {
      ZIO.succeed(Set.empty)
    } else {
      val newTrace = trace + ctx.path
      ctx match {
        case ctx: Ctx.LetCtx =>
          walkDown(ctx.body, newTrace, emitted)
        case ctx: Ctx.FunCtx =>
          walkDown(ctx.body, newTrace, emitted)
            .map(_ diff ctx.params.toSet)
            .flatMap(emit(ctx))
        case ctx: Ctx.AppCtx =>
          ZIO
            .collectAll(ctx.args.map(s => walkDown(s, newTrace, emitted)))
            .map(_.reduce(_ ++ _))
        case ctx: Ctx.RefCtx =>
          val to = ctx.to
          ctx.resolveRef(archive)(to) match {
            case Some(ResolvedRef.IsCtx(xcope)) =>
              walkDown(xcope, newTrace, emitted)
            case Some(ResolvedRef.IsParam(_, param)) => ZIO.succeed(Set(param))
            case Some(_)                             => ZIO.succeed(Set.empty[DeclParam])
            case None =>
              ctx.failM(s"""Assembling book $book
                           |Reference to $to is not defined""".stripMargin)
          }
        case _ =>
          Task.succeed(Set.empty)
      }
    }
  }

  @tailrec
  def exportedPaths(root: Ctx): Map[String, Ctx] = {
    root.asInstanceOf[BaseCtx[_]] match {
      case ctx: Ctx.ImportCtx => exportedPaths(ctx.body)
      case ctx: Ctx.LetCtx    => exportedPaths(ctx.body)
      case ctx: Ctx.DictCtx   => ctx.items
      case _                  => throw new Exception(s"${root}It's not a valid module expression")
    }
  }

  def choosePaths(
      startingAt: Ctx,
      emit: zio.Ref[Map[Ctx, Set[DeclParam]]]
  ): IO[CompileError, Map[Ctx, Set[DeclParam]]] = {
    for {
      empty <- walkDown(startingAt, Set.empty, emit)
      _ <- if (empty.isEmpty) { Task.succeed(()) }
          else ZIO.die(new Exception("Never happens, i guess"))
      paths <- emit.get
    } yield paths.updated(startingAt, Set.empty)
  }

  def headerOf(node: Ctx, capturedParams: Seq[DeclParam]): Seq[DeclParam] = {
    capturedParams
      .appendedAll(node match {
        case scope: Ctx.FunCtx => scope.params
        case _                 => Nil
      })
      .sortBy(p => p.path.length -> p.index)
  }
  def calcShifters(
      namespace: String,
      chosenNodes: Map[Ctx, Seq[DeclParam]]
  ): IO[CompileError, Map[Vector[String], Shifter]] = {
    val indexed: Seq[(Ctx, Seq[DeclParam])] = chosenNodes.toSeq.sortBy {
      case (s, _) => s.path.length
    }

    val bundles: IndexedSeq[NodeBundle] = indexed.map {
      case (ctx, params) =>
        val header  = headerOf(ctx, params)
        val idThunk = ctx.path.mkString(" ")
        NodeBundle(
          id = idThunk,
          ctx = ctx,
          capturedParamsSize = params.size,
          header = header,
          page = ThunkRef(foreign = false, book = namespace, id = idThunk)
        )
    }.toIndexedSeq

    val indexedBundles: Map[Vector[String], NodeBundle] = bundles.map(b => b.ctx.path -> b).toMap

    def invocationShift(scope: Ctx, enclosing: NodeBundle): IO[CompileError, Shift] = {
      indexedBundles
        .get(scope.path)
        .map { bundle =>
          ZIO
            .effect {
              Shift.Apply(
                Seq(Shift.Archive(bundle.page)).appendedAll(
                  bundle.header
                    .take(bundle.capturedParamsSize)
                    .map { usedParam =>
                      val i = enclosing.header.indexOf(usedParam)
                      i.ensuring(
                        i >= 0, {
                          "Assertion failed GONORREA"
                        }
                      )
                      Shift.Arg(i)
                    }
                )
              )
            }
            .mapError(e => CompileError.ParsingError(s" oops2 ${e.getMessage}", None))
        }
        .getOrElse(toShift(scope, enclosing))

    }

    def toShift(scope: Ctx, bundle: NodeBundle): IO[CompileError, Shift] = {
      scope match {
        case xcope: Ctx.FunCtx =>
          invocationShift(xcope.body, bundle)
        case lexcope: Ctx.ImportCtx =>
          invocationShift(lexcope.body, bundle)
        case lexcope: Ctx.DictCtx =>
          ZIO
            .foreach(lexcope.items.view.mapValues(x => invocationShift(x, bundle)).toList) {
              case (key, task) => task.map(key -> _)
            }
            .map { items =>
              items.foldLeft(
                Shift.Archive(foreign = true, module = "dicts", id = "empty"): Shift
              ) {
                case (accum, (key, shift)) =>
                  Shift.Apply(
                    Seq(
                      Shift.Archive(foreign = true, "dicts", "updated"),
                      accum,
                      Shift.Of(???),
                      shift
                    )
                  )
              }
            }
        case lexcope: Ctx.ArrCtx =>
          ZIO
            .collectAll(lexcope.items.map(x => invocationShift(x, bundle)).toList)
            .map { items =>
              items.foldLeft(Shift.Archive(foreign = true, "lists", "empty"): Shift) {
                case (accum, shift) =>
                  Shift.Apply(
                    Seq(Shift.Archive(foreign = true, "lists", "prepended"), accum, shift)
                  )
              }
            }
        case lexcope: Ctx.AppCtx =>
          ZIO
            .collectAll(lexcope.args.map(x => invocationShift(x, bundle)))
            .map { rrr =>
              Shift.Apply(rrr)
            }
        case lexcope: Ctx.LetCtx =>
          invocationShift(lexcope.body, bundle)
        case lexcope: Ctx.RefCtx =>
          lexcope.resolveRef(archive)(lexcope.to) match {
            case Some(ResolvedRef.IsCtx(refdLexcope)) =>
              invocationShift(refdLexcope, bundle)
            case Some(ResolvedRef.IsParam(_, param)) =>
              ZIO
                .effect {
                  val i = bundle.header.indexOf(param)
                  i.ensuring(_ >= 0)
                  Shift.Arg(i)
                }
                .mapError(e => CompileError.ParsingError(s"Opps, ${e.getMessage}", None))
            case Some(ResolvedRef.IsImported(ref)) =>
              ZIO.succeed(Shift.Archive(ref))
            case None => lexcope.failM("alksjd")
          }
        case lexcope: Ctx.QRefCtx =>
          val book   = lexcope.book
          val member = lexcope.member
          archive
            .thunkRefTo(book, member)
            .map { ref =>
              ZIO.succeed(Shift.Archive(ref))
            }
            .getOrElse(
              ZIO.fail(CompileError.ParsingError(s"Thunk of ${book} and $member not found", None))
            )
        case lexcope: Ctx.TextCtx => ZIO.succeed(Shift.Of(Val.TextS(lexcope.value)))
        case lexcope: Ctx.FloatCtx =>
          ZIO.succeed(Shift.Of(Val.FloatS(lexcope.value)))
        case lexcope: Ctx.IntCtx  => ZIO.succeed(Shift.Of(Val.IntS(lexcope.value)))
        case lexcope: Ctx.BoolCtx => ZIO.succeed(Shift.Of(Val.BoolS(lexcope.value)))
        case lexcope =>
          ZIO.fail(CompileError.ParsingError(s"We are not handling yet this: ${lexcope}", None))
      }
    }

    ZIO
      .collectAll(bundles.map { bundle =>
        toShift(bundle.ctx, bundle).map { shift =>
          bundle.ctx.path -> Shifter(arity = bundle.header.length, shift)
        }
      })
      .map(_.toMap)
  }
  def assemble: IO[CompileError, Draft] = {
    logger.info(s"Started assembling module $book")
    for {
      exported: Map[String, Ctx] <- IO
                                     .effect(exportedPaths(Ctx.fromNode(root)))
                                     .mapError(e => CompileError.ParsingError(e.getMessage, None))
      emit: Ref[Map[Ctx, Set[DeclParam]]] <- Ref.make[Map[Ctx, Set[DeclParam]]](Map.empty)
      _ <- ZIO.collectAll_(exported.values.map { node =>
            choosePaths(node, emit)
          })
      paths <- emit.get
                .map(_.view.mapValues(_.toSeq.sortBy(p => p.path.length -> p.index)).toMap)
                .map { paths: Map[Ctx, Seq[DeclParam]] =>
                  val exportedOnes = exported.values.map(_ -> Seq.empty[DeclParam]).toSeq
                  (paths.toSeq ++ exportedOnes).toMap
                }
      shifters: Map[Vector[String], Shifter] <- calcShifters(book, paths)
    } yield {
      Draft(
        book,
        exported.map {
          case (member: String, ctx: Ctx) =>
            member -> ctx.path.mkString(" ")
        },
        shifters.map {
          case (key, value) => key.mkString(" ") -> value
        }
      )
    }
  }
}

object Assembler {
  def make(deps: Archive) = new DefaultAssembler(deps)
}
