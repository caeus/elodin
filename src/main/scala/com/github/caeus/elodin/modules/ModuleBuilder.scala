package com.github.caeus.elodin.modules

import com.github.caeus.elodin.modules.ArgsAs.PolyFolder
import com.github.caeus.elodin.modules.ModuleContainer.AnyOps
import com.github.caeus.elodin.runtime.Val.{Atomic, Fn, FnPointer}
import com.github.caeus.elodin.runtime.{EloSystem, Val}
import com.github.caeus.plutus.TPrepend
import zio.{RIO, Task, UIO}

import scala.language.implicitConversions
import scala.reflect.ClassTag

sealed trait ArgsAs[X] {
  def arity: Int
  final def ::[Y, R](arg: ValAs[Y])(implicit concat: TPrepend.Aux[Y, X, R]): ArgsAs[R] = {
    PolyFolder[Y, X, R](arg, this, concat.apply)
  }
  def accept(args: Seq[Val]): RIO[EloSystem, X]
}
sealed trait ValAs[X] extends ArgsAs[X] {
  final val arity: Int                                = 1
  final def accept(args: Seq[Val]): RIO[EloSystem, X] = RIO.effectSuspend(accept(args.head))
  def accept(value: Val): RIO[EloSystem, X]
}
sealed trait AtomicAs[X] extends ValAs[X] {
  def acceptAtomic(atomic: Val.Atomic): Task[X]
  final def accept(value: Val): RIO[EloSystem, X] =
    RIO
      .environment[EloSystem]
      .flatMap(_.atomize(value))
      .flatMap(acceptAtomic)
}
sealed trait AtomAs[X] extends AtomicAs[X] {
  def acceptAtom(atom: Val.Atom): Task[X]
  final def acceptAtomic(atomic: Val.Atomic): Task[X] =
    atomic match {
      case atom @ Val.Atom(_) => acceptAtom(atom)
      case _                  => Task.fail(new Exception("Is not atomic"))
    }
}

object ArgsAs {
  def fn: ValAs[FnPointer] =
    new AtomicAs[FnPointer] {
      override def acceptAtomic(atomic: Atomic): Task[FnPointer] =
        atomic match {
          case r @ Fn(pointer, args) => Task.succeed(r)
          case _                     => Task.fail(new Exception("is Not a function"))
        }
    }

  case class PolyFolder[H, T, V](head: ValAs[H], tail: ArgsAs[T], mix: (H, T) => V)
      extends ArgsAs[V] {
    override def accept(args: Seq[Val]): RIO[EloSystem, V] =
      RIO
        .effectSuspend(head.accept(args.head))
        .zipPar(RIO.effectSuspend(tail.accept(args.tail)))
        .map(mix.tupled)

    override lazy val arity: Int = head.arity + tail.arity
  }

  def is[X: ClassTag]: ValAs[X] =
    new AtomAs[X] {
      override def acceptAtom(atom: Val.Atom): Task[X] =
        implicitly[ClassTag[X]]
          .unapply(atom.of)
          .map { x => Task.succeed(x) }
          .getOrElse(
            Task.fail(
              new Exception(
                s"It's not of type ${implicitly[ClassTag[X]].runtimeClass.getCanonicalName}"
              )
            )
          )
    }

  def ?? : ValAs[Val] =
    new ValAs[Val] {
      override def accept(value: Val): RIO[EloSystem, Val] = RIO.succeed(value)
    }

  def !! : ValAs[Atomic] =
    new AtomicAs[Atomic] {
      override def acceptAtomic(atomic: Atomic): Task[Atomic] = Task.succeed(atomic)
    }

}
trait ModuleContainer {
  final implicit def richT[T](value: T): AnyOps[T]        = new AnyOps[T](value)
  final def ?? : ValAs[Val]                               = ArgsAs.??
  final def is[X: ClassTag]: ValAs[X]                     = ArgsAs.is[X]
  final def !! : ValAs[Atomic]                            = ArgsAs.!!
  final def isFn: ValAs[FnPointer]                          = ArgsAs.fn
  final def array: ValAs[Seq[Val]]                        = ???
  final def dict: ValAs[Map[String, Val]]                 = ???
  final def pointer(id: String): Val.Lazy                 = ???
  final def pointer(module: String, id: String): Val.Lazy = ???
  def name: String
  def define: ModuleBuilder => ModuleInst
}
object ModuleContainer {
  class AnyOps[T](private val value: T) extends AnyVal {
    def asAtom: UIO[Val.Atom] = Task.succeed(Val.Atom(value))
  }
}

final class ModuleBuilder(moduleName: String) {

  def apply[X](name: String)(argsAs: ArgsAs[X])(f: X => Task[Val]): ModuleInst = {
    ModuleInst.DefFolder(
      name,
      EloFolder(
        argsAs.arity,
        { args =>
          argsAs.accept(args).flatMap(f)
        }
      )
    )
  }
}

sealed trait ModuleInst {
  final def toInsts: Seq[ModuleInst] =
    this match {
      case ModuleInst.Multi(insts) => insts
      case a                       => Seq(a)
    }
  final def ~(other: ModuleInst): ModuleInst =
    ModuleInst.Multi(this.toInsts.appendedAll(other.toInsts))
}
object ModuleInst {
  case class DefFolder(name: String, folder: EloFolder) extends ModuleInst
  case class Multi(insts: Seq[ModuleInst])              extends ModuleInst
}
