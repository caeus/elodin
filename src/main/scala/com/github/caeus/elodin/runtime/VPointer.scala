package com.github.caeus.elodin.runtime

import com.github.caeus.elodin.archive.CalculationRef
import com.github.caeus.elodin.runtime.VPointer.Strict
import zio.{Ref, RefM, Task, ZIO}

sealed trait VPointer {
  def strict(atomizer: Atomizer): Task[Strict]
}

object VPointer {

  final class StrictPointer(value: Strict) extends VPointer {
    override def strict(atomizer: Atomizer): Task[Strict] = ZIO.succeed(value)
  }
  final class LazyPointer(ref: RefM[Either[FunS, Strict]]) extends VPointer {
    override def strict(atomizer: Atomizer): Task[Strict] =
      ref.modifySome(Null) {
        case Left(fun) => ???
      }
  }

  sealed trait Strict
  final case object Null                                            extends Strict
  final case object Illegal                                         extends Strict
  final case class TextS(value: String)                             extends Strict
  final case class IntS(value: BigInt)                              extends Strict
  final case class FloatS(value: BigInt)                            extends Strict
  final case class BoolS(value: Boolean)                            extends Strict
  final case class ListS(head: VPointer, tail: VPointer)            extends Strict
  final case class VectorS(value: Vector[VPointer])                 extends Strict
  final case class DictS(items: Map[String, VPointer])              extends Strict
  final case class FunS(page: CalculationRef, args: List[VPointer]) extends Strict
  final case class Foreign(value: Any)                              extends Strict
}
