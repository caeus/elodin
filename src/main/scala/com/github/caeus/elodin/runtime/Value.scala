package com.github.caeus.elodin.runtime

import com.github.caeus.elodin.archive.{CalculationRef, HArgs}
import io.circe.{Json, JsonNumber, JsonObject}

sealed trait Value {}
object Value {
  sealed trait Applicable extends Value {
    def applyTo(args: Seq[Value]): Value
    final def apply(args: Value*): Value = applyTo(args)
  }
  sealed trait Atomic extends Value
  case class Lazy(pointer: CalculationRef, args: Seq[Value]) extends Applicable {
    require(args != null)
    override def applyTo(args: Seq[Value]): Applicable = {
      Lazy(pointer, this.args.appendedAll(args))
    }
  }
  case class Atom(of: Any) extends Atomic {
    require(!of.isInstanceOf[Atomic], of.toString)
    require(!of.isInstanceOf[HArgs], of.toString)
  }
  case class Fun(pointer: CalculationRef, args: Seq[Value]) extends Atomic with Applicable {
    override def applyTo(args: Seq[Value]): Applicable =
      Lazy(pointer, this.args.appendedAll(args))
  }

  private def _fromJson(json: Json): Value =
    json.fold[Value](
      Value.Atom(()),
      Value.Atom.apply,
      (n: JsonNumber) => {
        n.toBigInt.orElse(n.toBigDecimal).map(Value.Atom.apply).get
      },
      Value.Atom.apply,
      { (seq: Seq[Json]) =>
        Value.Atom(seq.map(_fromJson))
      },
      (obj: JsonObject) => Value.Atom(obj.toVector.map(x => x._1 -> _fromJson(x._2)).toMap)
    )

  def fromJson(jsons: String): Either[Throwable, Value] =
    io.circe.parser
      .parse(jsons)
      .map(_fromJson)
}
