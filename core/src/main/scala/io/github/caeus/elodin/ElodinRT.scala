package io.github.caeus.elodin

import io.github.caeus.elodin.core.{Archive, EvalError, FromVal, ToVal, Val, ValRefWrapper}
import io.github.caeus.elodin.discipline.{
  Discipline,
  EffectFail,
  EffectSucceed,
  EffectSuspend,
  EffectVal
}
import io.github.caeus.elodin.runtime.RTError
import zio.{IO, ZIO}

trait ElodinRT {
  def eval: ElodinEval
  def archive: Archive
  def run(book: String, member: String, args: ValRefWrapper*): IO[RTError, Val]
}

final class DefaultElodinRT(discipline: Discipline, val eval: ElodinEval) extends ElodinRT {

  def asEffectVal(book: String, member: String, args: ValRefWrapper*): IO[EvalError, EffectVal] =
    eval
      .eval(book, member, args: _*)
      .map(_.to[EffectVal])
      .flatMap { either =>
        ZIO
          .fromEither(either)
          .mapError(errors => EvalError(errors.mkString("\n"), None))
      }

  override def run(book: String, member: String, args: ValRefWrapper*): IO[RTError, Val] = {
    (for {
      _      <- ZIO.unit
      effect <- asEffectVal(book, member, args: _*)
      _ = effect match {
           case EffectSucceed(value)                           =>
           case EffectFail(value)                              =>
           case EffectSuspend(input, whenSuccess, whenFailure) =>
         }
    } yield ???).mapError(_ => new Exception("")).orDie
  }

  override def archive: Archive = eval.archive
}

object ElodinRT {
}
