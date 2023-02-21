package io.github.caeus.elodin.eni

import io.github.caeus.elodin.eval.Eval.Kernel
import io.github.caeus.elodin.eval.{ENI, EvalException}
import io.github.caeus.elodin.value.Value
import zio.ZIO

trait ENIMod {
  private[eni] def value: Map[String, HostFhunk]

  final def bindings: ZIO[Kernel, EvalException, Map[String, Value]] =
    ZIO
      .collectAll(value.map {
        case (k, _) =>
          ZIO
            .service[ENI]
            .flatMap { eni =>
              eni.reduce(k, Nil)
            }
            .map(k -> _)

      })
      .map(v => v.toMap)
}
final case class MergeENIMod(value: Map[String, HostFhunk]) extends ENIMod {}
object ENIMod {
  def merge(mods: ENIMod*): Either[Set[String], Map[String, HostFhunk]] = {
    def rec(
        mods: List[ENIMod],
        result: Map[String, HostFhunk]
    ): Either[Set[String], Map[String, HostFhunk]] = {
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
  def apply(values: (String, HostFhunk)*): ENIMod = MergeENIMod(values.toMap)
}
