package io.github.caeus.felurian.eni

import io.github.caeus.felurian.eval.EvalException
import io.github.caeus.felurian.value.Value
import io.github.caeus.felurian.value.Value.Applicable
import zio.ZIO

object FunKernelMod extends KernelMod {
  override def value: Map[String, Fhunk] =
    Map(
      "andThen" -> Fhunk.make { (f0: Applicable) => (f1: Applicable) => (arg: Value) =>
        f0(arg, Nil).flatMap { r =>
          f1(r, Nil)
        }
      },
      "orElse" -> Fhunk.make { (f0: Applicable) => (f1: Applicable) => (arg: Value) =>
        f0(arg, Nil).catchSome {
          case EvalException.Undefined(_) => f1(arg, Nil)
        }
      },
      "flip" -> Fhunk.make { (fn: Applicable) => (a0: Value) => (a1: Value) =>
        fn(a1, Seq(a0))
      },
      "identity" -> Fhunk.make { (v: Value) =>
        v
      },
      "assert" -> Fhunk.make { (b: Boolean) =>
        if (b) {
          ZIO.succeed(true)
        } else
          EvalException.undefined("Assertion not met")
      }
    )
}
