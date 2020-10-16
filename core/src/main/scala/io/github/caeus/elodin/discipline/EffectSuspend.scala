package io.github.caeus.elodin.discipline

import io.github.caeus.elodin.core.{Closure, FromVal, ToVal, Val}
import io.github.caeus.elodin.discipline.XXX._
import io.github.caeus.elodin.generic._
case class EffectInput(book: String, technique: String, args: List[Val])

object XXX {
  implicit class AnyOptToVal[+T](private val value: T) extends AnyVal {
    def toVal(implicit toVal: ToVal[T]) = toVal.cast(value)
  }
}

object EffectInput {

  implicit val toVal: ToVal[EffectInput] = (t: EffectInput) =>
    Val.DictS(
      Map(
        "book"      -> t.book.toVal,
        "technique" -> t.technique.toVal,
        "args"      -> t.args.toVal
      )
    )
}

sealed trait EffectVal
object EffectVal {
  implicit val fromVal: FromVal[EffectVal] = FromVal[EffectSuspend]
    .orElse[EffectSucceed]
    .map(_.fold(identity, identity))
    .orElse[EffectFail]
    .map(_.fold(identity, identity))
}
case class EffectSucceed(value: Val) extends EffectVal
object EffectSucceed {
  implicit val toVal: ToVal[EffectSucceed] = auto.to.gen[EffectSucceed].tagged("eff", "succeed")
  implicit val fromVal: FromVal[EffectSucceed] =
    auto.from.gen[EffectSucceed].tagged("eff", "succeed")
}
case class EffectFail(value: Val) extends EffectVal
object EffectFail {
  implicit val toVal: ToVal[EffectFail]     = auto.to.gen[EffectFail].tagged("eff", "fail")
  implicit val fromVal: FromVal[EffectFail] = auto.from.gen[EffectFail].tagged("eff", "fail")
}
case class EffectSuspend(
    input: EffectInput,
    whenSuccess: Closure,
    whenFailure: Closure
) extends EffectVal
object EffectSuspend {

  implicit val toVal: ToVal[EffectSuspend] = auto.to.gen[EffectSuspend].tagged("eff", "suspend")

  implicit val fromVal: FromVal[EffectSuspend] =
    auto.from.gen[EffectSuspend].tagged("eff", "suspend")
}
