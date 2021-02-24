package io.github.caeus.felurian.runtime

import io.github.caeus.felurian.runtime.Ctx.FunCtx
import io.github.caeus.felurian.value.Value
import io.github.caeus.felurian.value.Value.{ForeignFunVal, FunVal}
import zio.ZIO

object ApplyTo {

  private def continue(
      result: Value,
      remaining: Seq[Value]
  ): ZIO[ModuleSystem, EvalException, Value] = {
    remaining.headOption
      .map { arg0 =>
        result.applicable(arg0, remaining.tail)
      }
      .getOrElse(ZIO.succeed(result))
  }

  def foreign(
      name: String,
      arity: Int,
      reduce: Seq[Value] => ZIO[ModuleSystem, EvalException, Value]
  )(
      arg0: Value,
      args: Seq[Value]
  ): ZIO[ModuleSystem, EvalException, Value] = {
    apply(arity, reduce, args => ForeignFunVal(name, args))(arg0, args)
  }

  def ctx(ctx: FunCtx)(
      arg0: Value,
      args: Seq[Value]
  ): ZIO[ModuleSystem, EvalException, Value] = {
    apply(
      arity = ctx.arity,
      reduce = args => ctx.reduce(args).eval,
      starved = args => FunVal(ctx, args)
    )(
      arg0,
      args
    )
  }

  private def apply(
      arity: Int,
      reduce: Seq[Value] => ZIO[ModuleSystem, EvalException, Value],
      starved: Seq[Value] => Value
  )(arg0: Value, args: Seq[Value]) = {
    val (taken, remaining) = args.prepended(arg0).splitAt(arity)
    if (taken.size < arity) {
      ZIO.succeed(starved(taken))
    } else {
      reduce(taken).flatMap { result =>
        continue(result, remaining)
      }
    }
  }
}
