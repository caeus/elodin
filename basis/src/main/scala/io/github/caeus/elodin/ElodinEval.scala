package io.github.caeus.elodin

import io.github.caeus.elodin.core._
import zio.{IO, ZIO}

trait ElodinEval {
  def archive: Archive
  def eval(ref: ThunkRef, args: List[ValRef]): IO[EvalError, Val]
  final def get(book: String, member: String): IO[EvalError, ValRef] = {
    ZIO.effectSuspendTotal {
      archive
        .thunkRefTo(book, member)
        .map { ref =>
          ValRef.fromThunkRef(ref)
        }
        .getOrElse(ZIO.fail(EvalError(s"There's no member $book $member", None)))
    }
  }
  final def eval(book: String, member: String, args: ValRefWrapper*): IO[EvalError, Val] = {
    ZIO.effectSuspendTotal {
      archive
        .thunkRefTo(book, member)
        .map { ref =>
          eval(ref, args.map(_.value).toList)
        }
        .getOrElse(ZIO.fail(EvalError(s"There's no member $book $member", None)))
    }
  }
}
