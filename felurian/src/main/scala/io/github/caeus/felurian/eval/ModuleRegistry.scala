package io.github.caeus.felurian.eval

import io.github.caeus.felurian.compile.CompiledModule
import io.github.caeus.felurian.util.LilThrowable
import zio.{Ref, Task, UIO, ZIO, ZLayer}

trait ModuleRegistry {
  def register(module: EvaluatedModule): Task[Unit]
  def byName(name: String): UIO[Option[EvaluatedModule]]
}

object ModuleRegistry {
  def make = {
    Ref.make[Map[String, EvaluatedModule]](Map.empty).map { ref =>
      new LiveModuleRegistry(ref): ModuleRegistry
    }
  }
  val live = ZLayer.fromEffect(make)
}
final class LiveModuleRegistry(ref: Ref[Map[String, EvaluatedModule]]) extends ModuleRegistry {
  override def register(module: EvaluatedModule): Task[Unit] = {
    ref
      .modify {
        case modules if !modules.contains(module.name) =>
          true -> modules.updated(module.name, module)
        case modules => false -> modules
      }
      .flatMap {
        case true =>
          ZIO.unit
        case false =>
          ZIO.fail(new LilThrowable(s"Module with name ${module.name} already registered"))

      }
  }

  override def byName(name: String): UIO[Option[EvaluatedModule]] = {
    ref.get.map(_.get(name))
  }
}
