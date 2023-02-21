package io.github.caeus.elodin.eval

import io.github.caeus.elodin.value.Value
import io.github.caeus.elodin.value.Value.ImplRef
import zio.ZIO

object Eval {

  type Kernel = ENI & Archive
  def apply(
      fn: Value,
      arg0: Value,
      args: Seq[Value]
  ): ZIO[Kernel, EvalException, Value] = {
    fn match {
      case Value.FunVal(implRef, accumulated) =>
        eval(implRef, accumulated.appended(arg0).appendedAll(args))
      case _ => EvalException.applyError(fn)
    }
  }

  def knownArity(
      arity: Int,
      starved: Seq[Value] => Value,
      exact: Seq[Value] => ZIO[
        Kernel,
        EvalException,
        Value
      ]
  )(args: Seq[Value]) = {
    if (args.size < arity) {
      ZIO.succeed(starved(args))
    } else {
      val (taken, remaining) = args.splitAt(arity)
      exact(taken).flatMap { fn =>
        remaining.headOption
          .map { arg0 =>
            apply(fn, arg0, remaining.tail)
          }
          .getOrElse(ZIO.succeed(fn))
      }
    }
  }

  def eval(
      implRef: ImplRef,
      args: Seq[Value]
  ): ZIO[Kernel, EvalException, Value] = {
    implRef match {
      case ImplRef.Host(to) =>
        ZIO.service[ENI].flatMap(_.reduce(to, args))
      case ImplRef.Guest(module, index, scope) =>
        ZIO.service[Archive].flatMap(_.reduce(module, index, scope, args))
    }
  }
}
