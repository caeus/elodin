package com.github.caeus.elodin.runtime

import com.github.caeus.elodin.compiler.EloScript
import com.github.caeus.elodin.runtime.Val.{Atomic, FnPointer}
import com.github.caeus.elodin.types.EloEffect
import zio.{Task, TaskManaged, ZManaged}

trait EffectEngine {
  def exec(desc: EloEffect.EffectDesc): Task[Either[Val, Val]]

}
trait EloFunction {
  def apply(value: Val): Task[Atomic]
}

trait EloRuntime {

  def register(script: EloScript): Task[Unit]
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
          eval(fn.applyTo(Seq(value)))
        }
      case _ => Task.fail(new Exception("laksjdlkasd"))
    }
  }

  def eval(value: Val): Task[Atomic] = {
    system.eval(value)
  }

  override def register(script: EloScript): Task[Unit] = system.register(script)
}

object EloRuntime {
  def make(effectEngine: EffectEngine): TaskManaged[EloRuntime] = {
    ZManaged.fromEffect(EloSystem.make.map { system =>
      new DefaultEloRuntime(system, effectEngine)
    })
  }
}
