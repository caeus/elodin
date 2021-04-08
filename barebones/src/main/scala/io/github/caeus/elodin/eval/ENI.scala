package io.github.caeus.elodin.eval

import io.github.caeus.elodin.eni.{HostFhunk, StdENIMod}
import io.github.caeus.elodin.value.Value
import io.github.caeus.elodin.value.Value.{FunVal, ImplRef}
import zio.{Has, ZIO, ZLayer}

object ENI {
  type Box = Has[Service]
  trait Service {
    def reduce(
        to: String,
        args: Seq[Value]
    ): ZIO[Archive.Box, EvalException, Value]
  }

  private final class LiveService extends Service {
    private val kernelMod = StdENIMod

    override def reduce(
        to: String,
        args: Seq[Value]
    ): ZIO[Archive.Box, EvalException, Value] = {
      kernelMod.value
        .get(to)
        .map { reducer: HostFhunk =>
          Eval.knownArity(
            reducer.arity,
            args => FunVal(ImplRef.Host(to), accumulated = args),
            reducer.reducer
          )(args)
        }
        .getOrElse(EvalException.referenceError(to))
        .provideSome[Archive.Box](_.add(this: Service))
    }
  }

  def make: Service = new LiveService
  val live          = ZLayer.succeed(make)

}
