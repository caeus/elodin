package io.github.caeus.elodin.compile

import io.github.caeus.elodin.ElodinEval
import io.github.caeus.elodin.core._
import zio.{RIO, URIO, ZIO}

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
  case class FunParam(declParam: DeclParam)              extends RefResolution
  case class LetBinding(name: String, to: Lexcope2[Node]) extends RefResolution
  case class ModuleMember(ref: ThunkRef)                 extends RefResolution
}

case class DeclParam(path: Dig.Path, index: Int)
sealed trait Shift {
  private final def applyRec(values: Seq[ValRef])(shift: Shift): URIO[ElodinEval, ValRef] = {
    shift match {
      case Shift.Of(to) => URIO.succeed(ValRef.fromVal(to))
      case Shift.Arg(index) =>
        RIO
          .effect(values(index))
          .catchAll(_ => ZIO.succeed(ValRef.error("Wrong index")))
      case Shift.Apply(shifts: Seq[Shift]) =>
        URIO
          .collectAll(shifts.map(applyRec(values)))
          .map(_.toList)
          .flatMap {
            case fa :: applicants =>
              URIO
                .environment[ElodinEval]
                .flatMap(eval => fa.memoEval(eval))
                .flatMap {
                  case fun: Val.FunS => fun.applyTo(applicants)
                  case p =>
                    ZIO.succeed(ValRef.error(s"Trying to apply non function $p to $applicants"))
                }
                .catchAll((e: EvalError) => ZIO.succeed(ValRef.error("Error applying in shift", e)))
            case Nil => RIO.succeed(ValRef.fromVal(Val.UnitS))
          }
      case Shift.Archive(thunk) =>
        ValRef.fromPage(thunk, Nil)
    }
  }
  final def apply(values: Seq[ValRef]): URIO[ElodinEval, ValRef] = {
    applyRec(values)(this)
  }
}
object Shift {
  case class Of(value: Val)           extends Shift
  case class Apply(args: Seq[Shift])  extends Shift
  case class Arg(index: Int)          extends Shift
  case class Archive(thunk: ThunkRef) extends Shift
  object Archive {
    def apply(module: String, id: String): Archive = Archive(ThunkRef(module, id))
  }
  def of[T: ToVal](t: T): Shift = Of(ToVal[T].cast(t))
}
