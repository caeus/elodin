package com.github.caeus.elodin.archive

import com.github.caeus.elodin.compile.{Lexcope, Node}
import com.github.caeus.elodin.runtime.Value.Applicable
import com.github.caeus.elodin.runtime.{Atomizer, Value}
import zio.RIO

case class Shifter(arity: Int, shift: Shift) {}

case class ModuleInit(name: String, shifters: IndexedSeq[Shifter])

sealed trait Dig {
  override def toString: String =
    this match {
      case Dig.Down         => "_"
      case Dig.Key(value)   => value
      case Dig.Index(value) => s"[$value]"
    }
}
object Dig {
  type Path = Seq[Dig]
  case object Down              extends Dig
  case class Key(value: String) extends Dig
  case class Index(value: Int)  extends Dig

}

sealed trait RefResolution
object RefResolution {
  case class FunParam(declParam: DeclParam)               extends RefResolution
  case class LetBinding(name: String, to: Lexcope[Node])  extends RefResolution
  case class ModuleMember(module: String, member: String) extends RefResolution
}

case class DeclParam(path: Dig.Path, index: Int)
sealed trait Shift {
  private final def applyRec(values: Seq[Value])(shift: Shift): RIO[Atomizer, Value] = {
    shift match {
      case Shift.Of(to)     => RIO.succeed(to)
      case Shift.Arg(index) => RIO.effect(values(index))
      case Shift.Apply(shifts: Seq[Shift]) =>
        RIO
          .collectAll(shifts.map(applyRec(values)))
          .flatMap {
            case (fn: Applicable) :: args =>
              RIO.succeed(fn.applyTo(args))
            case Nil => RIO.succeed(Value.Atom(()))
            case _   => RIO.fail(new Exception("NOT APLICABLE"))
          }
      case Shift.Archive(page) =>
        RIO.succeed(Value.Lazy(page, Nil))
    }
  }
  final def apply(values: Seq[Value]): RIO[Atomizer, Value] = {
    applyRec(values)(this)
  }
}
object Shift {
  case class Of(value: Value)              extends Shift
  case class Apply(args: Seq[Shift])       extends Shift
  case class Arg(index: Int)               extends Shift
  case class Archive(page: CalculationRef) extends Shift
  object Archive {
    def apply(module: String, id: Int): Archive = Archive(CalculationRef(module, id))
  }
}
