package io.github.caeus.felurian.runtime.ffi

import io.github.caeus.felurian.runtime.ToValue.EvalIO
import io.github.caeus.felurian.runtime.TypedArg.of
import io.github.caeus.felurian.runtime._
import io.github.caeus.felurian.value.Value
import io.github.caeus.felurian.value.Value.Applicable
import zio.ZIO

object ListKernelMod extends KernelMod {
  override def value: Map[String, NativeReducer[Nat]] =
    Map(
      "::" ->
        Signature(of[Value] :: of[Seq[Value]] :: _).impl { head => tail =>
          tail.prepended(head)
        },
      "List#get" -> Signature(of[Seq[Value]] :: of[BigInt] :: _).impl { list => pos =>
        list
          .lift(pos.toInt)
          .map(Right.apply[EvalException, Value])
          .getOrElse(Left(EvalException.Undefined("Out of bounds")))
      },
      "List#empty" -> Signature(identity).impl {
        Nil: Seq[Value]
      },
      "List#map" -> Signature(of[Seq[Value]] :: of[Applicable] :: _).impl { list => f =>
        ZIO.collectAll(list.map { i =>
          f(i, Nil)
        }): ZIO[ModuleSystem, EvalException, Seq[Value]]
      },
      "List#foldl" -> Signature(of[Seq[Value]] :: of[Value] :: of[Applicable] :: _).impl {
        list => start => f =>
          list.foldLeft(ZIO.succeed(start): EvalIO[Value]) { (b: EvalIO[Value], value: Value) =>
            b.flatMap { b =>
              f(b, Seq(value))
            }
          }: EvalIO[Value]
      },
      "List#foldr" -> Signature(of[Seq[Value]] :: of[Value] :: of[Applicable] :: _).impl {
        list => start => f =>
          list.foldRight(ZIO.succeed(start): EvalIO[Value]) { (value, b) =>
            b.flatMap { b =>
              f(value, Seq(b))
            }
          }: EvalIO[Value]
      }
    )
}
