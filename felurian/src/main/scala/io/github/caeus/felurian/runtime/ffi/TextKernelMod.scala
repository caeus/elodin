package io.github.caeus.felurian.runtime.ffi

import io.github.caeus.felurian.runtime.Signature.of
import io.github.caeus.felurian.runtime.{Nat, NativeReducer, Signature}

object TextKernelMod extends KernelMod {
  override def value: Map[String, NativeReducer[Nat]] =
    Map(
      "++" -> Signature(of[String] :: of[String] :: _).impl { s0 => s1 =>
        s0 ++ s1
      },
      "charAt" -> Signature(of[String] :: of[BigInt] :: _).impl { s => pos =>
        s.charAt(pos.intValue).toString
      }
    )
}
