package io.github.caeus.felurian.runtime.ffi

import io.github.caeus.felurian.runtime.Signature.of
import io.github.caeus.felurian.runtime.{Nat, NativeReducer, Signature}

object BoolKernelMod extends KernelMod {
  def op(f: (Boolean, Boolean) => Boolean) =
    Signature(of[Boolean] :: of[Boolean] :: _).impl { a => b =>
      f(a, b)
    }
  override def value: Map[String, NativeReducer[_ <: Nat]] =
    Map(
      "&&" -> op(_ && _),
      "||" -> op(_ || _),
      "not" -> Signature(of[Boolean] :: _).impl { b =>
        !b
      }
    )
}
