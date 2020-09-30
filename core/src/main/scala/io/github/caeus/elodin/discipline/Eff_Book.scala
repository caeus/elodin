package io.github.caeus.elodin.discipline

import io.github.caeus.elodin.archive.{BookBuilder, TypedArg}
import io.github.caeus.elodin.archive.HArgs.#:

object Eff_Book {
  import TypedArg._
  val book = BookBuilder
    .withTitle("eff")
    .thunk("succeed")(
      _.at(value #: _).calculate {
        case value #: _ => EffectSucceed(value)
      }
    )
    .thunk("fail")(
      _.at(value #: _).calculate {
        case value #: _ => EffectFail(value)
      }
    )
    .build
}
