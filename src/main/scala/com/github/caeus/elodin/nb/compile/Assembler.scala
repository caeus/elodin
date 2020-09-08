package com.github.caeus.elodin.nb.compile

import com.github.caeus.elodin.compiler.RefResolution.{FunParam, LetBinding, ModuleMember}
import com.github.caeus.elodin.compiler.{DeclParam, Dig, Shift, Shifter}
import com.github.caeus.elodin.lang.Node
import com.github.caeus.elodin.nb.archive.Book.CBook
import com.github.caeus.elodin.nb.compile.Lexcope._
import com.github.caeus.elodin.nb.archive.{Archive, Book, CalculationRef}
import com.github.caeus.elodin.nb.runtime.Value
import com.github.caeus.elodin.runtime.{PPointer, Val}
import zio.{Ref, Task, ZIO}

trait Assembler {
  def assemble(name: String, node: Node): Task[Book]
}

final class DefaultAssembler(deps: Archive) extends Assembler {

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
                  case None                  => Task.fail(new Exception(s"Reference to $to is not defined"))
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
      case _                => throw new Exception("It's not a valid module expression")
    }
  }

  def choosePaths(node: Node): Task[Map[Lexcope[Node], Set[DeclParam]]] = {
    for {
      emit    <- Ref.make[Map[Lexcope[Node], Set[DeclParam]]](Map.empty)
      root     = Root(node)
      exported = exportedPaths(root)
      empty <- ZIO
                .collectAll(
                  exported.map {
                    case (name, node) => walkDown(node, Set.empty, emit)
                  }
                )
                .map(_.flatten)
      _ <- if (empty.isEmpty) { Task.succeed(()) }
          else Task.fail(new Exception("Never happens, i guess"))
      paths <- emit.get
    } yield paths.updated(root, Set.empty)
  }
  case class NodeBundle(
      id: Int,
      scope: Lexcope[Node],
      capturedParamsSize: Int,
      header: Seq[DeclParam],
      page: CalculationRef
  )
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
  ): Task[IndexedSeq[Shifter]] = {
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
                        println(enclosing)
                        println(usedParam)
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
          Shifter(arity = bundle.header.length, shift)
        }
      })
      .map(_.toIndexedSeq)
  }

  def assemble(namespace: String, node: Node) = {
    for {
      exported: Map[String, Lexcope[Node]] <- Task.effect(exportedPaths(Root(node)))

      paths <-
        choosePaths(node).map(_.view.mapValues(_.toSeq.sortBy(p => p.path.length -> p.index)).toMap)
      init <- calcShifters(namespace, paths)
    } yield CBook(namespace, exported.view.mapValues(_ => 1).toMap, init)
  }

}
object Assembler {
  def make(deps: Archive) = new DefaultAssembler(deps)
}
