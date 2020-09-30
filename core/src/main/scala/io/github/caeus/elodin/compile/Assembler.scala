package io.github.caeus.elodin.compile

import com.typesafe.scalalogging.LazyLogging
import RefResolution.{FunParam, LetBinding, ModuleMember}
import io.github.caeus.elodin.archive._
import io.github.caeus.elodin.basis.{Archive, Book, ThunkRef, Val}
import io.github.caeus.elodin.compile.Lexcope._
import zio.{IO, Ref, Task, ZIO}

trait Assembler {
  def assemble(name: String, node: Node): IO[CompileError, Book]
}

final class DefaultAssembler(deps: Archive) extends Assembler with LazyLogging {
  def assemble(book: String, node: Node): IO[CompileError, CBook] =
    new AssemblerVisitor(deps, book, node).assemble
}
case class NodeBundle(
    id: String,
    scope: Lexcope[Node],
    capturedParamsSize: Int,
    header: Seq[DeclParam],
    page: ThunkRef
)
final class AssemblerVisitor(archive: Archive, book: String, root: Node) extends LazyLogging {
  def walkDown(
      lexcope: Lexcope[Node],
      touchedPaths: Set[Dig.Path],
      emitted: Ref[Map[Lexcope[Node], Set[DeclParam]]]
  ): IO[CompileError, Set[DeclParam]] = {
    def emit(lexcope: Lexcope[Node])(capturedParams: Set[DeclParam]) = {
      emitted
        .update { map =>
          map.updated(lexcope, capturedParams)
        }
        .map(_ => capturedParams)
    }
    if (touchedPaths.contains(lexcope.path)) {
      ZIO.succeed(Set.empty)
    } else {
      val newTouchedPaths = touchedPaths + lexcope.path
      lexcope.widen match {
        case WhenLet(lexcope) =>
          walkDown(lexcope.body, newTouchedPaths, emitted)
        case WhenFn(lexcope) =>
          walkDown(lexcope.body, newTouchedPaths, emitted)
            .map(_ diff lexcope.params)
            .flatMap(emit(lexcope.widen))
        case WhenApply(xcope) =>
          ZIO
            .collectAll(xcope.args.map(s => walkDown(s, newTouchedPaths, emitted)))
            .map(_.reduce(_ ++ _))
        case WhenRef(lexcope) =>
          val to = lexcope.node.to
          for {
            resolution <- lexcope.resolve(to).provide(archive)
            r <- resolution match {
                  case Some(LetBinding(_, xcope)) =>
                    walkDown(xcope, touchedPaths, emitted)
                  case Some(FunParam(param)) => ZIO.succeed(Set(param))
                  case Some(_)               => ZIO.succeed(Set.empty[DeclParam])
                  case None =>
                    ZIO.fail(
                      CompileError(
                        s"""Assembling book $book
                       |Reference to $to is not defined""".stripMargin,
                        None
                      )
                    )
                }
          } yield r
        case _ => Task.succeed(Set.empty)
      }
    }
  }

  def exportedPaths(root: Lexcope[Node]): Map[String, Lexcope[Node]] = {
    root match {
      case WhenImport(node) => exportedPaths(node.body.widen)
      case WhenLet(node)    => exportedPaths(node.body.widen)
      case WhenDict(node)   => node.items
      case _                => throw new Exception(s"${root.node}It's not a valid module expression")
    }
  }

  def choosePaths(
      startingAt: Lexcope[Node],
      emit: Ref[Map[Lexcope[Node], Set[DeclParam]]]
  ): IO[CompileError, Map[Lexcope[Node], Set[DeclParam]]] = {
    for {
      empty <- walkDown(startingAt, Set.empty, emit)
      _ <- if (empty.isEmpty) { Task.succeed(()) }
          else ZIO.die(new Exception("Never happens, i guess"))
      paths <- emit.get
    } yield paths.updated(startingAt, Set.empty)
  }

  def headerOf(node: Lexcope[Node], capturedParams: Seq[DeclParam]): Seq[DeclParam] = {
    capturedParams
      .appendedAll(node match {
        case WhenFn(scope) => scope.params.toSeq
        case _             => Nil
      })
      .sortBy(p => p.path.length -> p.index)
  }
  def calcShifters(
      namespace: String,
      chosenNodes: Map[Lexcope[Node], Seq[DeclParam]]
  ): IO[CompileError, Map[Dig.Path, (String, Shifter)]] = {
    val indexed: Seq[(Lexcope[Node], Seq[DeclParam])] = chosenNodes.toSeq.sortBy {
      case (s, _) => s.path.length
    }

    val bundles: IndexedSeq[NodeBundle] = indexed.map {
      case (scope, params) =>
        val header = headerOf(scope, params)
        val str    = scope.path.toString()
        NodeBundle(str, scope, params.size, header, ThunkRef(namespace, str))
    }.toIndexedSeq

    val indexedBundles: Map[Dig.Path, NodeBundle] = bundles.map(b => b.scope.path -> b).toMap

    def invocationShift(scope: Lexcope[Node], enclosing: NodeBundle): IO[CompileError, Shift] = {
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
            .mapError(e => CompileError(s" oops2 ${e.getMessage}", None))
        }
        .getOrElse(toShift(scope, enclosing))

    }

    def toShift(scope: Lexcope[Node], bundle: NodeBundle): IO[CompileError, Shift] = {
      scope match {
        case WhenFn(xcope) =>
          invocationShift(xcope.body, bundle)
        case WhenImport(lexcope) =>
          invocationShift(lexcope.body, bundle)
        case WhenDict(lexcope) =>
          ZIO
            .foreach(lexcope.items.view.mapValues(x => invocationShift(x, bundle)).toList) {
              case (key, task) => task.map(key -> _)
            }
            .map { items =>
              items.foldLeft(Shift.Archive("predef", "dict\\empty"): Shift) {
                case (accum, (key, shift)) =>
                  Shift.Apply(
                    Seq(Shift.Archive("predef", "dict\\updated"), accum, Shift.Of(???), shift)
                  )
              }
            }
        case WhenArr(lexcope) =>
          ZIO
            .collectAll(lexcope.items.map(x => invocationShift(x, bundle)).toList)
            .map { items =>
              items.foldLeft(Shift.Archive("predef", "list\\empty"): Shift) {
                case (accum, shift) =>
                  Shift.Apply(
                    Seq(Shift.Archive("predef", "list\\prepend"), accum, shift)
                  )
              }
            }
        case WhenApply(lexcope) =>
          ZIO
            .collectAll(lexcope.args.map(x => invocationShift(x, bundle)))
            .map { rrr =>
              Shift.Apply(rrr)
            }
        case WhenLet(lexcope) =>
          invocationShift(lexcope.body, bundle)
        case WhenRef(lexcope) =>
          for {
            resolution <- lexcope.resolve(lexcope.node.to).map(_.get).provide(archive)
            r <- resolution match {
                  case LetBinding(_, refdLexcope) =>
                    invocationShift(refdLexcope, bundle)
                  case FunParam(param) =>
                    ZIO
                      .effect {
                        val i = bundle.header.indexOf(param)
                        i.ensuring(_ >= 0)
                        Shift.Arg(i)
                      }
                      .mapError(e => CompileError(s"Opps, ${e.getMessage}", None))
                  case ModuleMember(ref) =>
                    if (archive.contains(ref)) {
                      ZIO.succeed(Shift.Archive(ref))
                    } else {
                      ZIO.fail(CompileError(s"Member not found  $ref!", None))
                    }

                }
          } yield r
        case WhenQRef(lexcope) =>
          val book   = lexcope.node.book
          val member = lexcope.node.member
          if (archive.contains(ThunkRef(book, member))) {
            ZIO.fail(CompileError(s"Thunk of ${book} and $member not found", None))
          } else {
            ZIO.succeed(Shift.Archive(book, member))
          }
        case WhenText(lexcope) => ZIO.succeed(Shift.Of(Val.TextS(lexcope.node.value)))
        case WhenFloat(lexcope) =>
          ZIO.succeed(Shift.Of(Val.FloatS(lexcope.node.value)))
        case WhenInt(lexcope)  => ZIO.succeed(Shift.Of(Val.IntS(lexcope.node.value)))
        case WhenBool(lexcope) => ZIO.succeed(Shift.Of(Val.BoolS(lexcope.node.value)))
        case lexcope =>
          ZIO.fail(CompileError(s"We are not handling yet this: ${lexcope.node}", None))
      }
    }

    ZIO
      .collectAll(bundles.map { bundle =>
        toShift(bundle.scope, bundle).map { shift =>
          bundle.scope.path -> (bundle.id, Shifter(arity = bundle.header.length, shift))
        }
      })
      .map(_.toMap)
  }
  def assemble: IO[CompileError, CBook] = {
    logger.info(s"Started assembling module $book")
    for {
      exported: Map[String, Lexcope[Node]] <- IO
                                               .effect(exportedPaths(Root(root)))
                                               .mapError(e => CompileError(e.getMessage, None))
      emit: Ref[Map[Lexcope[Node], Set[DeclParam]]] <-
        Ref.make[Map[Lexcope[Node], Set[DeclParam]]](Map.empty)
      _ <- ZIO.collectAll_(exported.values.map { node =>
            choosePaths(node, emit)
          })
      paths <- emit.get
                .map(_.view.mapValues(_.toSeq.sortBy(p => p.path.length -> p.index)).toMap)
                .map { paths: Map[Lexcope[Node], Seq[DeclParam]] =>
                  val exportedOnes = exported.values.map(_ -> Seq.empty[DeclParam]).toSeq
                  ((paths.toSeq ++ exportedOnes)).toMap
                }
      shifters: Map[Dig.Path, (String, Shifter)] <- calcShifters(book, paths)
    } yield {
      CBook(
        book,
        exported.map {
          case (member: String, lexcope: Lexcope[Node]) =>
            member -> shifters(lexcope.path)._1
        },
        shifters.values.toMap
      )
    }
  }
}

object Assembler {
  def make(deps: Archive) = new DefaultAssembler(deps)
}
