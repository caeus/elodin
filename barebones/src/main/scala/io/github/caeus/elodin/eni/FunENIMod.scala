package io.github.caeus.elodin.eni

import io.github.caeus.elodin.eval.EvalException
import io.github.caeus.elodin.value.Value
import io.github.caeus.elodin.value.Value.Applicable
import zio.ZIO

object FunENIMod extends ENIMod {
  override def value: Map[String, HostFhunk] =
    Map(
      "andThen" -> HostFhunk.make { (f0: Applicable) =>(f1: Applicable) =>(arg: Value) =>
        f0(arg, Nil).flatMap { r =>
          f1(r, Nil)
        }
      },
      "orElse" -> HostFhunk.make { (f0: Applicable) =>(f1: Applicable) =>(arg: Value) =>
        f0(arg, Nil).catchSome {
          case EvalException.Undefined(_) => f1(arg, Nil)
        }
      },
      "flip" -> HostFhunk.make { (fn: Applicable) =>(a0: Value) =>(a1: Value) =>
        fn(a1, Seq(a0))
      },
      "identity" -> HostFhunk.make { (v: Value) =>
        v
      },
      "assert" -> HostFhunk.make { (b: Boolean) =>
        if (b) {
          ZIO.succeed(true)
        } else
          EvalException.undefined("Assertion not met")
      }
    )
}
