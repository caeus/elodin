package io.github.caeus.elodin.basis

import io.github.caeus.elodin.ElodinEval
import zio.ZIO

final case class Thunk(arity: Int, calc: List[ValRef] => ZIO[ElodinEval, EvalError, Val])
