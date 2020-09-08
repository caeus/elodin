package com.github.caeus.elodin.nb

import com.github.caeus.elodin.nb.archive.{Archive, Book}
import com.github.caeus.elodin.nb.runtime.Effect.Suspend
import com.github.caeus.elodin.nb.runtime.PopResult.{Complete, Incomplete}
import com.github.caeus.elodin.nb.runtime.Value.Atomic
import com.github.caeus.elodin.nb.runtime.{Atomizer, EStack, Effect, Response, Value}
import zio.{Task, ZIO}

trait Elodin extends Atomizer {
  def get(book: String, chapter: String): Task[Value]
  def run(value: Value): Task[Response]
  def atomize(value: Value): Task[Atomic]
}
final class DefaultElodin(dependencies: Archive) extends Elodin {
  override def get(book: String, chapter: String): Task[Value] =
    dependencies.page(book, chapter).map { page =>
      Value.Lazy(page, Nil)
    }

  private def run(effect: Effect): Task[Response] = {
    effect match {
      case Suspend(description, args, onSuccess, onFailure) =>
        dependencies
          .actionAt(description)
          .flatMap { perform =>
            perform.form(args.toList).provide(this)
          }
          .flatMap {
            case Right(value) => run(onSuccess(value))
            case Left(value)  => run(onFailure(value))
          }
      case Effect.Result(r) =>
        Task.effect(Response.Result(r))
    }
  }

  override def run(value: Value): Task[Response] = {
    for {
      atom <- atomize(value)
      r <- atom match {
            case Value.Atom(of: Effect) =>
              run(of)
            case Value.Atom(_) =>
              ZIO.fail(new Exception("Expected to run an effect, got a something else instead"))
            case Value.Fun(pointer, args) =>
              ZIO.fail(new Exception("Expected to run an effect, got a Function instead"))
          }
    } yield {
      println(r)
      r
    }
  }

  private def _atomize(value: Value, stack: EStack[Value]): Task[Atomic] = {
    value match {
      case atom: Atomic => Task.succeed(atom)
      case Value.Lazy(pointer, args) =>
        for {
          _      <- stack.pushAll(args)
          folder <- dependencies.calculationAt(pointer)
          newVal <- stack
                     .pop(folder.arity)
                     .flatMap {
                       case Complete(els) => folder.cast(els)
                       case Incomplete(els) =>
                         ZIO.succeed(Value.Fun(pointer, els))
                     }
                     .provide(this)
          r <- _atomize(newVal, stack)
        } yield r
    }
  }
  override def atomize(value: Value): Task[Atomic] = {
    EStack.make[Value](Nil).flatMap(stack => _atomize(value, stack))
  }
}
object Elodin {
  def make(deps: Seq[Book]): Task[Elodin] =
    Archive.make(deps).map { deps =>
      new DefaultElodin(deps): Elodin
    }
}
