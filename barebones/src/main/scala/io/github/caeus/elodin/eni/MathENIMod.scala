package io.github.caeus.elodin.eni

import io.github.caeus.elodin.eval.ToValueIO
import io.github.caeus.elodin.value.Value

object Math {}

object MathENIMod extends ENIMod {

  def op[BD, BI](bd: (BigDecimal, BigDecimal) => BD, bi: (BigInt, BigInt) => BI)(implicit
      toValueD: ToValueIO[BD],
      toValueI: ToValueIO[BI]
  ) = {

    HostFhunk
      .make(bd)
      .orElse {
        HostFhunk.make(bi)
      }
  }
  override def value: Map[String, HostFhunk] =
    Map(
      "(+)"  -> op[BigDecimal, BigInt](_ + _, _ + _),
      "(*)"  -> op[BigDecimal, BigInt](_ * _, _ * _),
      "(/)"  -> op[BigDecimal, BigInt](_ / _, _ / _),
      "(-)"  -> op[BigDecimal, BigInt](_ - _, _ - _),
      "(>)"  -> op[Boolean, Boolean](_ > _, _ > _),
      "(<)"  -> op[Boolean, Boolean](_ < _, _ < _),
      "(<=)" -> op[Boolean, Boolean](_ <= _, _ <= _),
      "(>=)" -> op[Boolean, Boolean](_ >= _, _ >= _),
      "isNumber" -> HostFhunk.make { (arg: Value) =>
        arg match {
          case Value.IntVal(_)   => true
          case Value.FloatVal(_) => true
          case _                 => false
        }
      },
      "isFloat" -> HostFhunk.make { (arg: Value) =>
        arg match {
          case Value.FloatVal(_) => true
          case _                 => false
        }
      },
      "isInt" -> HostFhunk.make { (arg: Value) =>
        arg match {
          case Value.IntVal(_) => true
          case _               => false
        }
      }
    )
}
