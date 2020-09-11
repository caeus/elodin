package com.github.caeus.elodin.nb

import com.github.caeus.elodin.nb.archive.{Archive, Book, PredefArchive}
import com.github.caeus.elodin.nb.runtime.Effect.Suspend
import com.github.caeus.elodin.nb.runtime.PopResult.{Complete, Incomplete}
import com.github.caeus.elodin.nb.runtime.Value.Atomic
import com.github.caeus.elodin.nb.runtime.{Atomizer, EStack, Effect, PathdAtomizer, Response, Value}
import com.typesafe.scalalogging.LazyLogging
import zio.{Task, ZIO}

import scala.util.Random

trait Elodin {
  def get(book: String, chapter: String): Task[Atomic]
  def run(value: Value): Task[Response]
  def atomize(value: Value): Task[Atomic]
}
final class DefaultElodin(archive: Archive) extends Elodin with LazyLogging {
  override def get(book: String, chapter: String): Task[Atomic] =
    archive
      .page(book, chapter)
      .map { page =>
        Value.Lazy(page, Nil)
      }
      .flatMap(atomize)

  private def run(effect: Effect): Task[Response] = {
    effect match {
      case Suspend(op, onSuccess, onFailure) =>
        logger.info(s"Running $op")
        archive
          .actionAt(op.ref)
          .flatMap { perform =>
            perform.form(op.args.toList).provide(new PathdAtomizer(archive, Nil))
          }
          .flatMap {
            case Right(value) => run(onSuccess(value))
            case Left(value)  => run(onFailure(value))
          }
      case Effect.Done(r) =>
        logger.info(s"Succeeded with $r")
        Task.effect(Response.Result(r))
    }
  }

  override def run(value: Value): Task[Response] = {
    for {
      atom <- atomize(value)
      r <- atom match {
            case Value.Atom(of: Effect) =>
              run(of)
            case Value.Atom(s) =>
              ZIO.fail(new Exception(s"Expected to run an effect, got $s instead"))
            case Value.Fun(_, _) =>
              ZIO.fail(new Exception("Expected to run an effect, got a Function instead"))
          }
    } yield {
      r
    }
  }

  override def atomize(value: Value): Task[Atomic] = {
    new PathdAtomizer(archive, Nil).atomize(value)
  }
}
object Elodin {
  def make(deps: Seq[Book]): Task[Elodin] =
    Archive.make(deps).map { deps =>
      new DefaultElodin(deps): Elodin
    }
  def default(extraBooks: Seq[Book]) = {
    PredefArchive.archiveM
      .map(_.enrichedWith(extraBooks))
      .map(archive => new DefaultElodin(archive))
  }
}
