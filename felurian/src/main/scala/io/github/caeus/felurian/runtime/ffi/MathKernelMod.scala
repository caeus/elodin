package io.github.caeus.felurian.runtime.ffi

import io.github.caeus.felurian.runtime.TypedArg.of
import io.github.caeus.felurian.runtime.{Nat, NativeReducer, Signature, ToValue}
import io.github.caeus.felurian.value.Value

object MathKernelMod extends KernelMod {

  def op[BD, BI](bd: (BigDecimal, BigDecimal) => BD, bi: (BigInt, BigInt) => BI)(implicit
      toValueD: ToValue[ToValue.EvalIO, BD],
      toValueI: ToValue[ToValue.EvalIO, BI]
  ) = {
    Signature(of[BigDecimal] :: of[BigDecimal] :: _)
      .impl { num1 => num2 =>
        bd(num1, num2)
      }
      .orElse(
        Signature(of[BigInt] :: of[BigInt] :: _).impl { num1 => num2 =>
          bi(num1, num2)
        }
      )
      .orElse(
        Signature(of[BigInt] :: of[BigDecimal] :: _).impl { num1 => num2 =>
          bd(BigDecimal(num1), num2)
        }
      )
      .orElse(
        Signature(of[BigDecimal] :: of[BigInt] :: _).impl { num1 => num2 =>
          println(s"num1: $num1")
          println(s"num2: $num2")
          bd(num1, BigDecimal(num2))
        }
      )
  }
  override def value: Map[String, NativeReducer[_ <: Nat]] =
    Map(
      "+"  -> op[BigDecimal, BigInt](_ + _, _ + _),
      "*"  -> op[BigDecimal, BigInt](_ * _, _ * _),
      "/"  -> op[BigDecimal, BigInt](_ / _, _ / _),
      "-"  -> op[BigDecimal, BigInt](_ - _, _ - _),
      ">"  -> op[Boolean, Boolean](_ > _, _ > _),
      "<"  -> op[Boolean, Boolean](_ < _, _ < _),
      "<=" -> op[Boolean, Boolean](_ <= _, _ <= _),
      ">=" -> op[Boolean, Boolean](_ >= _, _ >= _),
      "isNumber" -> Signature(of[Value] :: _).impl {
        case _: Value.FloatVal => true
        case _: Value.IntVal   => true
        case _                 => false
      },
      "isFloat" -> Signature(of[Value] :: _).impl {
        case _: Value.FloatVal => true
        case _                 => false
      },
      "isInt" -> Signature(of[Value] :: _).impl {
        case _: Value.IntVal => true
        case _               => false
      }
    )
}
