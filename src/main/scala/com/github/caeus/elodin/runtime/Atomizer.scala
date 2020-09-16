package com.github.caeus.elodin.runtime

import com.github.caeus.elodin.archive.{Archive, CalculationRef}
import PopResult.{Complete, Incomplete}
import Value.{Atomic, Fun}
import com.github.caeus.elodin.runtime.VPointer.{FunS, Strict}
import com.github.caeus.elodin.runtime.Value.Atomic
import zio.{Task, ZIO}

sealed trait Atomizer {
  def get(book: String, chapter: String): Task[Value.Lazy]
  def atomize(value: Value): Task[Atomic]
  def atomize2(page: CalculationRef, args: List[VPointer]): Task[Strict]
}
final class PathdAtomizer(archive: Archive, path: List[String]) extends Atomizer {

  def deeper(s: String): PathdAtomizer = new PathdAtomizer(archive, s :: path)
  override def get(book: String, chapter: String): Task[Value.Lazy] =
    archive
      .chapterRef(book, chapter)
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

  private def _atomize(ref: CalculationRef, stack: EStack[VPointer]): Task[Strict] = {
    for {
      _         <- ZIO.unit
      calculate <- archive.calculationAt(ref)
    } yield ???
  }
  override def atomize2(page: CalculationRef, args: List[VPointer]): Task[Strict] = {

    ???
  }
}
