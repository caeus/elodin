package io.github.caeus.felurian.eni

import io.github.caeus.felurian.eval.{EvalException, Resolver}
import io.github.caeus.felurian.value.Value
import io.github.caeus.felurian.value.Value.ForeignFunVal
import zio.{IO, ZIO}

trait KernelMod {
  private[eni] def value: Map[String, Fhunk]

  final def bindings: ZIO[Resolver, EvalException, Map[String, Value]] =
    ZIO
      .collectAll(value.map {
        case (k, v) =>
          val arity   = v.arity
          val reducer = v.reducer
          (if (arity == 0) {
             reducer(Nil)
           } else {
             IO.succeed(ForeignFunVal(k, Nil))
           }).map(k -> _)

      })
      .map(v => v.toMap)
}
final case class MergeKernelMod(value: Map[String, Fhunk]) extends KernelMod {}
object KernelMod {

  def merge(mods: KernelMod*): Either[Set[String], Map[String, Fhunk]] = {
    def rec(
        mods: List[KernelMod],
        result: Map[String, Fhunk]
    ): Either[Set[String], Map[String, Fhunk]] = {
      mods match {
        case mod :: mods =>
          val collision = mod.value.keySet.intersect(result.keySet)
          if (collision.isEmpty) {
            rec(mods, result ++ mod.value)
          } else {
            Left(collision)
          }
        case Nil => Right(result)
      }
    }
    rec(mods.toList, Map.empty)
  }
  def apply(values: (String, Fhunk)*): KernelMod = MergeKernelMod(values.toMap)
}
