package io.github.caeus.elodin.value

import io.github.caeus.elodin.eval.Eval.Kernel
import io.github.caeus.elodin.eval.{Eval, EvalException, Scope}
import zio.ZIO

sealed trait Value {}
object Value {
  sealed trait ImplRef
  object ImplRef {
    final case class Host(to: String)                                extends ImplRef
    final case class Guest(module: String, index: Int, scope: Scope) extends ImplRef
  }
  sealed trait Applicable { self: Value =>
    def apply(arg0: Value, args: Seq[Value]): ZIO[Kernel, EvalException, Value]
  }
  final case class IntVal(value: BigInt) extends Value {}

  final case class FloatVal(value: BigDecimal)                          extends Value
  final case class TextVal(value: String)                               extends Value
  final case class BoolVal(value: Boolean)                              extends Value
  final case class DictVal(value: Map[String, Value])                   extends Value
  final case class ListVal(value: Seq[Value])                           extends Value
  final case class TaggedVal(module: String, tag: String, value: Value) extends Value
  final case class FunVal(implRef: ImplRef, accumulated: Seq[Value]) extends Value with Applicable {
    override def apply(arg0: Value, args: Seq[Value]): ZIO[Kernel, EvalException, Value] = {
      Eval.eval(implRef, accumulated.appended(arg0).appendedAll(args))
    }
  }

  final case object UnitVal extends Value

  def fromEson(eson: Eson): Value =
    eson match {
      case Eson.IntVal(value)   => Value.IntVal(value)
      case Eson.FloatVal(value) => Value.FloatVal(value)
      case Eson.TextVal(value)  => Value.TextVal(value)
      case Eson.BoolVal(value)  => Value.BoolVal(value)
      case Eson.DictVal(value)  => Value.DictVal(value.view.mapValues(e => fromEson(e)).toMap)
      case Eson.ListVal(value)  => Value.ListVal(value.map(e => fromEson(e)))
      case Eson.UnitVal         => Value.UnitVal
    }

  def typeOf(value: Value): String =
    value match {
      case _: Applicable           => "function"
      case IntVal(_)               => "int"
      case FloatVal(_)             => "float"
      case TextVal(_)              => "text"
      case BoolVal(_)              => "bool"
      case DictVal(_)              => "dict"
      case ListVal(_)              => "list"
      case TaggedVal(module, _, _) => s"tagged($module)"
      case UnitVal                 => "unit"
    }
  def toEson(value: Value): Either[List[String], Eson] = {
    value match {
      case IntVal(value)   => Right(Eson.IntVal(value))
      case FloatVal(value) => Right(Eson.FloatVal(value))
      case TextVal(value)  => Right(Eson.TextVal(value))
      case BoolVal(value)  => Right(Eson.BoolVal(value))
      case DictVal(value) =>
        Eson
          .foldLeftEither(value.toList.map {
            case (k, v) => toEson(v).map(k -> _)
          })(Map.empty[String, Eson]) {
            case (map, (k, v)) => map.updated(k, v)
          }
          .map(v => Eson.DictVal(v))
      case ListVal(value) =>
        Eson
          .foldLeftEither(value.map(toEson).toList)(Vector.empty[Eson]) { (list, v) =>
            list.appended(v)
          }
          .map(v => Eson.ListVal(v))
      case TaggedVal(_, _, _) => Left(List("tagged values cannot be converted to Eson"))
      case FunVal(_, _)       => Left(List("functions cannot be converted to Eson"))
      case UnitVal            => Right(Eson.UnitVal)
    }
  }

}
