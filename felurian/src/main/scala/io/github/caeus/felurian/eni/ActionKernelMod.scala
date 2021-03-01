package io.github.caeus.felurian.eni
import io.github.caeus.felurian.eval.TaggedValue
import io.github.caeus.felurian.value.Value
import io.github.caeus.felurian.value.Value.Applicable

sealed trait Action
object Action {
  final case class FlatMap(fa: Value, a2fb: Applicable)     extends Action
  final case class Succeed(result: Value)                   extends Action
  final case class Fail(error: Value)                       extends Action
  final case class RecoverWith(fa: Value, a2fb: Applicable) extends Action
}

object ActionKernelMod extends KernelMod {

  override private[eni] def value =
    Map(
      "action#flatMap" -> Fhunk.make { (action: TaggedValue["action", "action", Value]) =>
        false
      },
      "action#map"         -> ???,
      "action#fail"        -> ???,
      "action#succeed"     -> ???,
      "action#recover"     -> ???,
      "action#recoverWith" -> ???
    )
}
