package io.github.caeus.elodin.eni

import io.github.caeus.elodin.eval.Eval.Kernel
import io.github.caeus.elodin.eval._
import io.github.caeus.elodin.value.Value
import io.github.caeus.elodin.value.Value.Applicable
import zio.ZIO

object ListENIMod extends ENIMod {
  override def value: Map[String, HostFhunk] =
    Map(
      "(::)" ->
        HostFhunk
          .make { (head: Value, tail: Seq[Value]) => tail.prepended(head) },
      "List#get" -> HostFhunk.make { (list: Seq[Value]) =>(pos: BigInt) =>
        list
          .lift(pos.toInt)
          .map(v => ZIO.succeed(v))
          .getOrElse(EvalException.undefined("Out of bounds"))
      },
      "List#empty" -> HostFhunk.make(Nil: Seq[Value]),
      "List#map" -> HostFhunk.make { (list: Seq[Value], f: Applicable) =>
        ZIO.collectAll(list.map { i =>
          f(i, Nil)
        }): ZIO[Kernel, EvalException, Seq[Value]]
      },
      "List#foldl" -> HostFhunk.make { (list: Seq[Value]) =>(start: Value) =>(f: Applicable) =>
        list.foldLeft(ZIO.succeed(start): ZIO[Kernel, EvalException, Value]) {
          (b: ZIO[Kernel, EvalException, Value], value: Value) =>
            b.flatMap { b =>
              f(b, Seq(value))
            }
        }: ZIO[Kernel, EvalException, Value]
      },
      "List#foldr" -> HostFhunk.make { (list: Seq[Value]) =>(start: Value) =>(f: Applicable) =>
        list.foldRight(ZIO.succeed(start): ZIO[Kernel, EvalException, Value]) { (value, b) =>
          b.flatMap { b =>
            f(value, Seq(b))
          }
        }: ZIO[Kernel, EvalException, Value]
      }
    )
}
