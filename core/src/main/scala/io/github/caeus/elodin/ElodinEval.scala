package io.github.caeus.elodin

import io.github.caeus.elodin.archive.{Archive, BookPageRef}
import io.github.caeus.elodin.runtime.PopResult.{Complete, Incomplete}
import io.github.caeus.elodin.runtime.Value.{Atomic, Fun}
import io.github.caeus.elodin.runtime.{EStack, Link, Piece, Value}
import zio.{IO, Task, UIO, ZIO}

sealed trait ElodinEval {
  def get(book: String, chapter: String): Task[Value.Lazy]
  def atomize(value: Value): Task[Atomic]
  def atomize2(page: BookPageRef, args: List[Link]): IO[Unit, Piece]
}
final class CtxEval(archive: Archive, path: List[String]) extends ElodinEval {
  def deeper(s: String): CtxEval = new CtxEval(archive, s :: path)
  override def get(book: String, chapter: String): Task[Value.Lazy] =
    archive
      .bookPageOf(book, chapter)
      .map { page =>
        Value.Lazy(page, Nil)
      }

  private def _atomize(value: Value, stack: EStack[Value]): Task[Atomic] = {
    value match {
      case atom: Atomic =>
        stack.elements.map(_.isEmpty).flatMap {
          case true => ZIO.succeed(atom)
          case false =>
            atom match {
              case Value.Atom(of) =>
                ZIO.fail(new Exception(s"Trying to apply arguments to non function $of"))
              case Fun(pointer, args) => _atomize(Value.Lazy(pointer, args), stack)
            }
        }
      case Value.Lazy(pointer, args) =>
        for {
          _      <- stack.pushAll(args)
          prname <- archive.realNameOf(pointer)
          //_       = println((prname :: path).reverse.mkString("[",",","]"))
          folder <- archive.calculationAt(pointer)
          newVal <- stack
                     .pop(folder.arity)
                     .flatMap {
                       case Complete(els) => folder.cast(els)
                       case Incomplete(els) =>
                         ZIO.succeed(Value.Fun(pointer, els))
                     }
                     .provide(deeper(prname))
          r <- _atomize(newVal, stack)
        } yield r
    }
  }
  override def atomize(value: Value): Task[Atomic] = {
    EStack
      .make[Value](Nil)
      .flatMap(stack => _atomize(value, stack))
  }

  def _atomize2(page: BookPageRef, stack: EStack[Link]): IO[Unit, Piece] = {
    ???
  }
  override def atomize2(page: BookPageRef, args: List[Link]): IO[Unit, Piece] = ???
}
object ElodinEval {
  def make(archive: Archive): UIO[ElodinEval] = UIO.succeed(new CtxEval(archive, Nil))
}
