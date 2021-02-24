package io.github.caeus.felurian.runtime.ffi

import io.github.caeus.felurian.runtime.Signature.of
import io.github.caeus.felurian.runtime.ToValue.AnyToValue
import io.github.caeus.felurian.runtime.{Nat, NativeReducer, Signature}
import io.github.caeus.felurian.value.Value

object WorkflowKernelMod extends KernelMod {
  override def value: Map[String, NativeReducer[Nat]] =
    Map(
      "Workflow#emit" -> Signature(of[String] :: of[Value] :: _).impl { name => details =>
        Map("name" -> name.toValueNow, "details" -> details)
          .taggedNow("Workflow", "emit")
      },
      "Workflow#build" -> ???
    )
}
