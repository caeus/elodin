package io.github.caeus.elodin

import io.github.caeus.elodin.core._
import zio.{IO, UIO}

trait ElodinEval {
  def archive: Archive
  final def get(ref: ThunkRef): UIO[ValRef] = ValRef.fromThunkRef(ref)
  def eval(ref: ThunkRef, args: List[ValRef]): IO[EvalError, Val]
  final def eval(book: String, member: String, args: ValRefWrapper*): IO[EvalError, Val] = {
    eval(ThunkRef(book, member), args.map(_.value).toList)
  }
}
