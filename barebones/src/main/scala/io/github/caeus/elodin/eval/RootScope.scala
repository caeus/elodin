package io.github.caeus.elodin.eval

import io.github.caeus.elodin.eni.StdENIMod
import io.github.caeus.elodin.eval.Eval.Kernel
import zio.{Has, ZIO}

object RootScope {

  type Box = Has[Service]
  trait Service {
    def forModule(module: String): ZIO[Kernel, EvalException, Scope]
  }
  private final class BarebonesService extends Service {
    override def forModule(module: String) =
      StdENIMod.bindings.map { scope =>
        Scope.fromMap(module, scope)
      }
  }
  def barebones: Service = {
    new BarebonesService
  }

}
