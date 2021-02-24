package io.github.caeus.felurian.value

import io.github.caeus.felurian.runtime.Ctx.{EvalVal, FunCtx}
import io.github.caeus.felurian.runtime.{ApplyTo, Ctx, EvalException, ModuleSystem}
import io.github.caeus.felurian.value.Value.Applicable
import zio.{IO, ZIO}

sealed trait Value {
  def applicable: Applicable =
    new Applicable {
      override def apply(arg0: Value, args: Seq[Value]): EvalVal = IO.fail(EvalException.ApplyError)
    }
}
object Value {

  sealed trait Applicable {
    def apply(arg0: Value, args: Seq[Value]): Ctx.EvalVal
  }
  final case class IntVal(value: BigInt) extends Value {}

  final case class FloatVal(value: BigDecimal)                               extends Value
  final case class TextVal(value: String)                                    extends Value
  final case class BoolVal(value: Boolean)                                   extends Value
  final case class DictVal(value: Map[String, Value])                        extends Value
  final case class ListVal(value: Seq[Value])                                extends Value
  final case class TaggedVal(module: String, tag: Seq[String], value: Value) extends Value
  final case class FunVal(ctx: FunCtx, accumulated: Seq[Value]) extends Value with Applicable {
    override def applicable = this: Applicable
    override def apply(arg0: Value, args: Seq[Value]): EvalVal = {
      val joined = accumulated.appended(arg0).appendedAll(args)
      ApplyTo.ctx(ctx)(joined.head, joined.tail)
    }
  }
  final case class ForeignFunVal(to: String, accumulated: Seq[Value])
      extends Value
      with Applicable {
    override def applicable = this: Applicable
    override def apply(arg0: Value, args: Seq[Value]): EvalVal = {
      ZIO.accessM[ModuleSystem] { system =>
        val joined = accumulated.appended(arg0).appendedAll(args)
        system.applyForeign(to, joined.head, joined.tail)
      }
    }
  }
  final case object UnitVal extends Value
}
