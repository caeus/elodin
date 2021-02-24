package io.github.caeus.felurian.runtime.ffi

import io.github.caeus.felurian.value.Value
import io.github.caeus.felurian.value.Value.Applicable
import io.github.caeus.felurian.runtime.{EvalException, Nat, NativeReducer, Signature}
import io.github.caeus.felurian.runtime.Signature.of

object FunKernelMod extends KernelMod {
  override def value: Map[String, NativeReducer[Nat]] =
    Map(
      "andThen" -> Signature(of[Applicable] :: of[Applicable] :: of[Value] :: _).impl {
        f0 => f1 => arg =>
          f0(arg, Nil).flatMap { r =>
            f1(r, Nil)
          }
      },
      "orElse" -> Signature(of[Applicable] :: of[Applicable] :: of[Value] :: _).impl {
        f0 => f1 => arg =>
          f0(arg, Nil).catchSome {
            case EvalException.Undefined(_) => f1(arg, Nil)
          }
      },
      "flip" -> Signature(of[Applicable] :: of[Value] :: of[Value] :: _).impl { fn => a0 => a1 =>
        fn(a1, Seq(a0))
      },
      "identity" -> Signature(of[Value] :: _).impl { v =>
        v
      },
      "assert" -> Signature(of[Boolean] :: _).impl {
        case true  => Right[EvalException, Boolean](true)
        case false => Left[EvalException, Boolean](EvalException.Undefined("Assertion not met"))
      }
    )
}
