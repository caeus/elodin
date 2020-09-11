package com.github.caeus.elodin.nb.compile

import com.github.caeus.elodin.compiler.RefResolution.{FunParam, LetBinding, ModuleMember}
import com.github.caeus.elodin.compiler.{DeclParam, Dig, Shift, Shifter}
import com.github.caeus.elodin.lang.Node
import com.github.caeus.elodin.nb.archive.Book.CBook
import com.github.caeus.elodin.nb.archive.{Archive, Book, CalculationRef}
import com.github.caeus.elodin.nb.compile.Lexcope._
import com.github.caeus.elodin.nb.runtime.Value
import com.typesafe.scalalogging.LazyLogging
import zio.{Ref, Task, ZIO}

trait Assembler {
  def assemble(name: String, node: Node): Task[Book]
}

final class DefaultAssembler(deps: Archive) extends Assembler with LazyLogging {

  def assemble(book: String, node: Node) = new AssemblerVisitor(deps, book, node).assemble
}
case class NodeBundle(
    id: Int,
    scope: Lexcope[Node],
    capturedParamsSize: Int,
    header: Seq[DeclParam],
    page: CalculationRef
)
final class AssemblerVisitor(deps: Archive, book: String, root: Node) extends LazyLogging {
  def walkDown(
      lexcope: Lexcope[Node],
      touchedPaths: Set[Dig.Path],
      emitted: Ref[Map[Lexcope[Node], Set[DeclParam]]]
  ): Task[Set[DeclParam]] = {
    def emit(lexcope: Lexcope[Node])(capturedParams: Set[DeclParam]) = {
      emitted
        .update { map =>
          map.updated(lexcope, capturedParams)
        }
        .map(_ => capturedParams)
    }
    if (touchedPaths.contains(lexcope.path)) {
      Task.succeed(Set.empty)
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
            resolution <- lexcope.resolve(to).provide(deps)
            r <- resolution match {
                  case Some(LetBinding(_, xcope)) =>
                    walkDown(xcope, touchedPaths, emitted)
                  case Some(FunParam(param)) => Task.succeed(Set(param))
                  case Some(_)               => Task.succeed(Set.empty[DeclParam])
                  case None =>
                    Task.fail(
                      new Exception(
                        s"""Assembling book $book
                       |Reference to $to is not defined""".stripMargin
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

  def choosePaths(startingAt: Lexcope[Node], emit: Ref[Map[Lexcope[Node], Set[DeclParam]]]) = {
    for {
      empty <- walkDown(startingAt, Set.empty, emit)
      _ <- if (empty.isEmpty) { Task.succeed(()) }
          else Task.fail(new Exception("Never happens, i guess"))
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
  ): Task[IndexedSeq[(Dig.Path, Shifter)]] = {
    val indexed: Seq[((Lexcope[Node], Seq[DeclParam]), Int)] = chosenNodes.toSeq.sortBy {
      case (s, _) => s.path.length
    }.zipWithIndex

    val bundles: IndexedSeq[NodeBundle] = indexed.map {
      case ((scope, params), id) =>
        val header = headerOf(scope, params)
        NodeBundle(id, scope, params.size, header, CalculationRef(namespace, id))
    }.toIndexedSeq

    val indexedBundles = bundles.map(b => b.scope.path -> b).toMap

    def invocationShift(scope: Lexcope[Node], enclosing: NodeBundle): Task[Shift] = {
      indexedBundles
        .get(scope.path)
        .map { bundle =>
          Task.effect {
            Shift.Apply(
              Seq(Shift.Of(Value.Lazy(bundle.page, Nil))).appendedAll(
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
        }
        .getOrElse(toShift(scope, enclosing))
    }

    def toShift(scope: Lexcope[Node], bundle: NodeBundle): Task[Shift] = {
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
              items.foldLeft(Shift.Archive("predef", 0): Shift) {
                case (accum, (key, shift)) =>
                  Shift.Apply(
                    Seq(Shift.Archive("predef", 0), accum, Shift.Of(Value.Atom(key)), shift)
                  )
              }
            }
        case WhenArr(lexcope) =>
          ZIO
            .collectAll(lexcope.items.map(x => invocationShift(x, bundle)).toList)
            .map { items =>
              items.foldLeft(Shift.Archive("predef", 0): Shift) {
                case (accum, shift) =>
                  Shift.Apply(
                    Seq(Shift.Archive("predef", 0), accum, shift)
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
            resolution <- lexcope.resolve(lexcope.node.to).map(_.get).provide(deps)
            r <- resolution match {
                  case LetBinding(_, refdLexcope) =>
                    invocationShift(refdLexcope, bundle)
                  case FunParam(param) =>
                    Task.effect {
                      val i = bundle.header.indexOf(param)
                      i.ensuring(_ >= 0)
                      Shift.Arg(i)
                    }
                  case ModuleMember(module, member) =>
                    deps.page(module, member).map { pointer =>
                      Shift.Archive(pointer)
                    }
                }
          } yield r
        case WhenQRef(lexcope) =>
          deps.page(lexcope.node.module, lexcope.node.member).map { pointer =>
            Shift.Archive(
              pointer.book,
              pointer.page
            )
          }
        case WhenText(lexcope)  => Task.succeed(Shift.Of(Value.Atom(lexcope.node.value)))
        case WhenFloat(lexcope) => Task.succeed(Shift.Of(Value.Atom(lexcope.node.value)))
        case WhenInt(lexcope)   => Task.succeed(Shift.Of(Value.Atom(lexcope.node.value)))
        case WhenBool(lexcope)  => Task.succeed(Shift.Of(Value.Atom(lexcope.node.value)))
        case lexcope            => Task.fail(new Exception(s"We are not handling yet this: ${lexcope.node}"))
      }
    }

    ZIO
      .collectAll(bundles.map { bundle =>
        toShift(bundle.scope, bundle).map { shift =>
          bundle.scope.path -> Shifter(arity = bundle.header.length, shift)
        }
      })
      .map(_.toIndexedSeq)
  }
  def assemble = {
    logger.info(s"Started assembling module $book")
    for {
      exported: Map[String, Lexcope[Node]] <- Task.effect(exportedPaths(Root(root)))
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
      shifters <- calcShifters(book, paths)
    } yield {
      val namespace1 = CBook(
        book,
        exported.view
          .mapValues(node => {
            val i = shifters.indexWhere {
              case (path, _) =>
                path == node.path
            }
            if (i >= 0) i else ???
          })
          .toMap,
        shifters.map(_._2)
      )
      logger.info(s"Finished assembling book $book")
      namespace1
    }
  }
}

object Assembler {
  def make(deps: Archive) = new DefaultAssembler(deps)
}
