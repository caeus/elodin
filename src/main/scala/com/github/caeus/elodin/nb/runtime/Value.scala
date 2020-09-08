package com.github.caeus.elodin.nb.runtime

import com.github.caeus.elodin.nb.archive.CalculationRef

sealed trait Value {}
object Value {
  sealed trait Applicable extends Value {
    def applyTo(args: Seq[Value]): Value
    final def apply(args: Value*): Value = applyTo(args)
  }
  sealed trait Atomic extends Value {}
  case class Lazy(pointer: CalculationRef, args: Seq[Value]) extends Applicable {
    require(args != null)
    override def applyTo(args: Seq[Value]): Applicable = {
      Lazy(pointer, this.args.appendedAll(args))
    }
  }
  case class Atom(of: Any) extends Atomic
  case class Fun(pointer: CalculationRef, args: Seq[Value]) extends Atomic with Applicable {
    override def applyTo(args: Seq[Value]): Applicable =
      Lazy(pointer, this.args.appendedAll(args))
  }

  def fromJson(jsons: String): Either[Throwable, Value] = Right(Atom(jsons))
}


