package io.github.caeus.felurian.runtime

import io.github.caeus.felurian.value.Value
import io.github.caeus.felurian.value.Value.ForeignFunVal
import io.github.caeus.felurian.runtime.ffi.StdKernelMod
import zio.IO

trait ModuleSystem {
  def module(module: String): IO[EvalException, Value]
  def moduleBindings(module: String): IO[EvalException, Map[String, Value]]
  def foreignValue(name: String): IO[EvalException, Value]
  def applyForeign(name: String, arg0: Value, args: Seq[Value]): IO[EvalException, Value]
}
final class LiveModuleSystem extends ModuleSystem {
  private val foreignSet = StdKernelMod

  override def module(module: String): IO[EvalException, Value] =
    IO.fail(EvalException.ReferenceError(s"""module: "$module""""))

  override def foreignValue(name: String): IO[EvalException, Value] = {
    foreignSet.value
      .get(name)
      .map { reducer =>
        if (reducer.arity.value == 0) {
          reducer.impl(Nil)
        } else {
          IO.succeed(ForeignFunVal(name, Nil))
        }
      }
      .getOrElse(IO.fail(EvalException.ReferenceError(name)))
      .provide(this)
  }

  override def applyForeign(
      name: String,
      arg0: Value,
      args: Seq[Value]
  ): IO[EvalException, Value] = {
    foreignSet.value
      .get(name)
      .map { reducer =>
        ApplyTo.foreign(name, reducer.arity.value, reducer.impl)(arg0, args)
      }
      .getOrElse(IO.fail(EvalException.ReferenceError(name)))
      .provide(this)
  }

  override def moduleBindings(module: String): IO[EvalException, Map[String, Value]] = {
    foreignSet.bindings
      .provide(this)
  }
}
