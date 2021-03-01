package io.github.caeus.felurian.eni

import io.github.caeus.felurian.eval._
import io.github.caeus.felurian.value.Value
import io.github.caeus.felurian.value.Value.Applicable
import zio.ZIO

object ListKernelMod extends KernelMod {
  override def value: Map[String, Fhunk] =
    Map(
      "::" ->
        Fhunk
          .make { (head: Value, tail: Seq[Value]) => tail.prepended(head) },
      "List#get" -> Fhunk.make { (list: Seq[Value]) => (pos: BigInt) =>
        list
          .lift(pos.toInt)
          .map(v => ZIO.succeed(v))
          .getOrElse(EvalException.undefined("Out of bounds"))
      },
      "List#empty" -> Fhunk.make(Nil: Seq[Value]),
      "List#map" -> Fhunk.make { (list: Seq[Value], f: Applicable) =>
        ZIO.collectAll(list.map { i =>
          f(i, Nil)
        }): ZIO[Resolver, EvalException, Seq[Value]]
      },
      "List#foldl" -> Fhunk.make { (list: Seq[Value]) => (start: Value) => (f: Applicable) =>
        list.foldLeft(ZIO.succeed(start): ZIO[Resolver, EvalException, Value]) {
          (b: ZIO[Resolver, EvalException, Value], value: Value) =>
            b.flatMap { b =>
              f(b, Seq(value))
            }
        }: ZIO[Resolver, EvalException, Value]
      },
      "List#foldr" -> Fhunk.make { (list: Seq[Value]) => (start: Value) => (f: Applicable) =>
        list.foldRight(ZIO.succeed(start): ZIO[Resolver, EvalException, Value]) { (value, b) =>
          b.flatMap { b =>
            f(value, Seq(b))
          }
        }: ZIO[Resolver, EvalException, Value]
      }
    )
}
