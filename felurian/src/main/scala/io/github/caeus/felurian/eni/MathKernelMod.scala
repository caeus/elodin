package io.github.caeus.felurian.eni

import io.github.caeus.felurian.eval.ToValueIO
import io.github.caeus.felurian.value.Value

object MathKernelMod extends KernelMod {

  def op[BD, BI](bd: (BigDecimal, BigDecimal) => BD, bi: (BigInt, BigInt) => BI)(implicit
      toValueD: ToValueIO[BD],
      toValueI: ToValueIO[BI]
  ) = {

    Fhunk
      .make(bd)
      .orElse {
        Fhunk.make(bi)
      }
  }
  override def value: Map[String, Fhunk] =
    Map(
      "+"  -> op[BigDecimal, BigInt](_ + _, _ + _),
      "*"  -> op[BigDecimal, BigInt](_ * _, _ * _),
      "/"  -> op[BigDecimal, BigInt](_ / _, _ / _),
      "-"  -> op[BigDecimal, BigInt](_ - _, _ - _),
      ">"  -> op[Boolean, Boolean](_ > _, _ > _),
      "<"  -> op[Boolean, Boolean](_ < _, _ < _),
      "<=" -> op[Boolean, Boolean](_ <= _, _ <= _),
      ">=" -> op[Boolean, Boolean](_ >= _, _ >= _),
      "isNumber" -> Fhunk.make { (arg: Value) =>
        arg match {
          case Value.IntVal(_)   => true
          case Value.FloatVal(_) => true
          case _                 => false
        }
      },
      "isFloat" -> Fhunk.make { (arg: Value) =>
        arg match {
          case Value.FloatVal(_) => true
          case _                 => false
        }
      },
      "isInt" -> Fhunk.make { (arg: Value) =>
        arg match {
          case Value.IntVal(_) => true
          case _               => false
        }
      }
    )
}
