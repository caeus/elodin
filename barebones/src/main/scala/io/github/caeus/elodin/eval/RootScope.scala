package io.github.caeus.elodin.eval

import io.github.caeus.elodin.eni.StdENIMod
import io.github.caeus.elodin.eval.Eval.Kernel
import zio.ZIO

trait RootScope {
  def forModule(module: String): ZIO[Kernel, EvalException, Scope]
}

private final class BarebonesRootScope extends RootScope {
  override def forModule(module: String): ZIO[Kernel, EvalException, Scope] =
    StdENIMod.bindings.map { scope =>
      Scope.fromMap(module, scope)
    }
}
object BarebonesRootScope {
  def live = new BarebonesRootScope()
}
