package com.github.caeus.elodin.runtime

import com.github.caeus.elodin.nb.Script
import com.github.caeus.elodin.runtime.Val.{Atomic, FnPointer}
import com.github.caeus.elodin.types.EloEffect
import zio.{Task, TaskManaged, ZManaged}

trait EloFunction {
  def apply(value: Val): Task[Either[Atomic,Atomic]]
}

trait EloRuntime {

  def register(script: Script): Task[Unit]
  def asFunction(value: String): Task[EloFunction]
}

final class DefaultEloRuntime(
    system: EloSystem,
    effectEngine: EffectEngine
) extends EloRuntime {

  override def asFunction(name: String): Task[EloFunction] = {
    system(name).flatMap {
      case fn: FnPointer =>
        Task.succeed { value: Val =>
          run(fn.applyTo(Seq(value)))
        }
      case _ => Task.fail(new Exception("laksjdlkasd"))
    }
  }

  def run(value: Val): Task[Either[Atomic,Atomic]] = {
    system.atomize(value).flatMap {
      case Val.Atom(EloEffect.Halt(desc, onSuccess, onFailure)) =>
        effectEngine
          .run(desc)
          .map {
            case Right(value) => onSuccess.applyTo(Seq(value))
            case Left(value)  => onFailure.applyTo(Seq(value))
          }
          .flatMap(run)
          .provide(system)
      case Val.Atom(EloEffect.Done(result)) => system.atomize(result).map(Right.apply)
      case Val.Atom(EloEffect.Failed(result)) => system.atomize(result).map(Left.apply)
      case _ => ???
    }
  }

  override def register(script: Script): Task[Unit] = system.register(script)
}

object EloRuntime {
  def make: TaskManaged[EloRuntime] = {
    ZManaged.fromEffect(EloSystem.make.map { system =>
      new DefaultEloRuntime(system, new DefaultEffectEngine)
    })
  }
}
