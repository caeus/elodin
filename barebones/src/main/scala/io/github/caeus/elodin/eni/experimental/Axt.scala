package io.github.caeus.elodin.eni.experimental

import io.github.caeus.elodin.eni.HostFhunk
import io.github.caeus.elodin.eni.MathENIMod.op
import io.github.caeus.elodin.value.Eson

class Axt {}

trait HostModule {
  def impl: PartialFunction[String, HostFhunk]
  def constant: PartialFunction[String, Eson]
}

import io.github.caeus.elodin.eval.ToValueIO
import io.github.caeus.elodin.value.Value

object MathHModule extends HostModule {
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
  override def impl =
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

  override def constant: PartialFunction[String, Eson] = ???
}
