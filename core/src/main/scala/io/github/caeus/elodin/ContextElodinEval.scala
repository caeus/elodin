package io.github.caeus.elodin

import io.github.caeus.elodin.core._
import io.github.caeus.elodin.runtime.{EStack, PopResult}
import zio.{IO, ZIO}

final class ContextElodinEval(val archive: Archive, path: List[ThunkRef]) extends ElodinEval {
  def deeper(ref: ThunkRef): ContextElodinEval = new ContextElodinEval(archive, ref :: path)

  def evalStack(ref: ThunkRef, stack: EStack[ValRef]): IO[EvalError, Val] = {
    for {
      _ <- if (path.size > 512)
            ZIO.fail(EvalError("TOO DEEP!", None))
          else ZIO.unit
      thunk <- ZIO
                .succeed(
                  archive
                    .thunkAt(ref)
                )
                .someOrFail(EvalError(s"No thunk at $ref", None))
      value <- stack
                .pop(thunk.arity)
                .flatMap {
                  case PopResult.Complete(els) =>
                    thunk
                      .calc(els)
                      .mapError(err => EvalError(s"Error Evaluating $ref", Some(err)))
                  case PopResult.Incomplete(els) => ZIO.succeed(Val.FunS(ref, els))
                }
                .provide(this.deeper(ref))
      value <- stack.isEmpty.flatMap {
                case true => ZIO.succeed(value)
                case false =>
                  value match {
                    case Val.FunS(page, args) =>
                      stack
                        .pushAll(args)
                        .flatMap { _ =>
                          evalStack(page, stack)
                        }
                    case p => ZIO.fail(EvalError(s"Trying to use $p as applicant", None))
                  }
              }
    } yield value
  }
  override def eval(ref: ThunkRef, args: List[ValRef]): IO[EvalError, Val] = {
    EStack.make[ValRef](args).flatMap { stack =>
      evalStack(ref, stack)
    }
  }
}
