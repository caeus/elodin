package io.github.caeus.elodin

import io.github.caeus.elodin.archive.{ActionRef, Archive, Book, PredefArchive}
import io.github.caeus.elodin.runtime.Effect.Suspend
import io.github.caeus.elodin.runtime.Value.{Applicable, Atomic}
import io.github.caeus.elodin.runtime.{Effect, PathdAtomizer, Value, Workflow}
import com.typesafe.scalalogging.LazyLogging
import io.circe.Json
import zio.{Task, ZIO}

trait Elodin {
  def get(book: String, chapter: String): Task[Atomic]
  def run(value: Value): Task[Workflow]
  def atomize(value: Value): Task[Atomic]
}
final class DefaultElodin(archive: Archive) extends Elodin with LazyLogging {
  override def get(book: String, chapter: String): Task[Atomic] =
    archive
      .chapterRef(book, chapter)
      .map { page =>
        Value.Lazy(page, Nil)
      }
      .flatMap(atomize)

  private def run(effect: Effect): Task[Workflow] = {
    effect match {
      case Suspend(op, onSuccess, onFailure) if op.ref == ActionRef("basis", "job.emit") =>
        ZIO.succeed(
          Workflow.Job(Json.fromString("lasjd"), onSuccess: Applicable, onFailure: Applicable)
        )
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
        Task.effect(Workflow.Done(r))
    }
  }

  override def run(value: Value): Task[Workflow] = {
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

  def make(archive: Archive): Task[Elodin] = {
    ZIO.succeed(new DefaultElodin(archive))
  }

  def make(deps: Seq[Book]): Task[Elodin] =
    Archive.make(deps).flatMap { deps =>
      make(deps)
    }
  def default(extraBooks: Seq[Book]) = {
    PredefArchive.archiveM
      .map(_.enrichedWith(extraBooks))
      .map(archive => new DefaultElodin(archive))
  }
}
