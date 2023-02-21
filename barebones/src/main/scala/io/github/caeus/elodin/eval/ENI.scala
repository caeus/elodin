package io.github.caeus.elodin.eval

import io.github.caeus.elodin.eni.{HostFhunk, StdENIMod}
import io.github.caeus.elodin.value.Value
import io.github.caeus.elodin.value.Value.{FunVal, ImplRef}
import zio.{ZIO, ZLayer}

trait ENI {
  def reduce(
      to: String,
      args: Seq[Value]
  ): ZIO[Archive, EvalException, Value]
}

private final class LiveENI extends ENI {
  private val kernelMod = StdENIMod

  override def reduce(
      to: String,
      args: Seq[Value]
  ): ZIO[Archive, EvalException, Value] = {
    kernelMod.value
      .get(to)
      .map { (reducer: HostFhunk) =>
        Eval.knownArity(
          reducer.arity,
          args => FunVal(ImplRef.Host(to), accumulated = args),
          reducer.reducer
        )(args)
      }
      .getOrElse(EvalException.referenceError(to))
      .provideSomeEnvironment[Archive](_.add(this: ENI))
  }
}

object LiveENI {

  def make: ENI = new LiveENI
  val live      = ZLayer.succeed(make)

}
