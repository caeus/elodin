package com.github.caeus.elodin.nb.runtime

import com.github.caeus.elodin.nb.archive.Archive
import com.github.caeus.elodin.nb.runtime.PopResult.{Complete, Incomplete}
import com.github.caeus.elodin.nb.runtime.Value.{Atomic, Fun}
import zio.{Task, ZIO}

sealed trait Atomizer {
  def get(book: String, chapter: String): Task[Value]
  def atomize(value: Value): Task[Atomic]
}
final class PathdAtomizer(dependencies: Archive, path: List[String]) extends Atomizer {

  def deeper(s: String): PathdAtomizer = new PathdAtomizer(dependencies, s :: path)
  override def get(book: String, chapter: String): Task[Value] =
    dependencies
      .page(book, chapter)
      .map { page =>
        Value.Lazy(page, Nil)
      }
      .flatMap(atomize)

  private def _atomize(value: Value, stack: EStack[Value]): Task[Atomic] = {
    value match {
      case atom: Atomic =>
        stack.elements.map(_.isEmpty).flatMap {
          case true => ZIO.succeed(atom)
          case false =>
            atom match {
              case Value.Atom(of) =>
                ZIO.fail(new Exception(s"Trying to apply arguments to non function $of"))
              case Fun(pointer, args) => _atomize(Value.Lazy(pointer, args),stack)
            }
        }
      case Value.Lazy(pointer, args) =>
        for {
          _      <- stack.pushAll(args)
          prname <- dependencies.realNameOf(pointer)
          //_       = println((prname :: path).reverse.mkString("[",",","]"))
          folder <- dependencies.calculationAt(pointer)
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

}
