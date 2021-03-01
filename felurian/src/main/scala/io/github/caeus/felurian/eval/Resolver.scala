package io.github.caeus.felurian.eval

import io.github.caeus.felurian.compile.Ast9
import io.github.caeus.felurian.eni.StdKernelMod
import io.github.caeus.felurian.value.Value
import io.github.caeus.felurian.value.Value.ForeignFunVal
import zio.{IO, ZIO}

//Elodin Native Interface
trait ENI {
  def rootBindings: ZIO[Resolver, EvalException, Map[String, Value]]
  def foreignValue(name: String): ZIO[Resolver, EvalException, Value]
  def applyForeign(
      name: String,
      arg0: Value,
      args: Seq[Value]
  ): ZIO[Resolver, EvalException, Value]
}

sealed trait Resolver {
  def module(name: String): ZIO[Resolver, EvalException, Value]
  // def moduleBindings(module: String): IO[EvalException, Map[String, Value]]
  def foreignValue(name: String): ZIO[Resolver, EvalException, Value]
  def applyForeign(
      name: String,
      arg0: Value,
      args: Seq[Value]
  ): ZIO[Resolver, EvalException, Value]
}
object Resolver {
  def make(evaluator: Evaluator, eni: ENI): Resolver = new Impl(evaluator, eni)
  private final class Impl(evaluator: Evaluator, eni: ENI) extends Resolver {
    override def module(name: String): ZIO[Resolver, EvalException, Value] =
      evaluator.module(name)
    override def foreignValue(name: String): ZIO[Resolver, EvalException, Value] =
      eni.foreignValue(name)

    override def applyForeign(
        name: String,
        arg0: Value,
        args: Seq[Value]
    ): ZIO[Resolver, EvalException, Value] =
      eni.applyForeign(name, arg0, args)
  }
}
final class LiveENI extends ENI {
  private val kernelMod = StdKernelMod

  override def foreignValue(name: String): ZIO[Resolver, EvalException, Value] = {
    kernelMod.value
      .get(name)
      .map { reducer =>
        if (reducer.arity == 0) {
          reducer.reducer(Nil)
        } else {
          IO.succeed(ForeignFunVal(name, Nil))
        }
      }
      .getOrElse(EvalException.referenceError(name))

  }

  override def applyForeign(
      name: String,
      arg0: Value,
      args: Seq[Value]
  ): ZIO[Resolver, EvalException, Value] = {
    kernelMod.value
      .get(name)
      .map { reducer =>
        ApplyTo.foreign(name, reducer.arity, reducer.reducer)(arg0, args)
      }
      .getOrElse(EvalException.referenceError(name))
  }

  override def rootBindings: ZIO[Resolver, EvalException, Map[String, Value]] =
    kernelMod.bindings
}
