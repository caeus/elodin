package io.github.caeus.felurian.runtime.ffi

import io.github.caeus.felurian.runtime.ToValue.EvalIO
import io.github.caeus.felurian.runtime.{Nat, NativeReducer}
import io.github.caeus.felurian.value.Value
import io.github.caeus.felurian.value.Value.ForeignFunVal
import zio.{IO, ZIO}

trait KernelMod {
  private[ffi] def value: Map[String, NativeReducer[Nat]]

  final def bindings: EvalIO[Map[String, Value]] =
    ZIO
      .collectAll(value.map {
        case (k, v) =>
          val arity   = v.arity.value
          val reducer = v.impl
          (if (arity == 0) {
             reducer(Nil)
           } else {
             IO.succeed(ForeignFunVal(k, Nil))
           }).map(k -> _)

      })
      .map(v => v.toMap)
}
final case class MergeKernelMod(value: Map[String, NativeReducer[Nat]]) extends KernelMod {}
object KernelMod {

  def merge(mods: KernelMod*): Either[Set[String], Map[String, NativeReducer[Nat]]] = {
    def rec(
        mods: List[KernelMod],
        result: Map[String, NativeReducer[Nat]]
    ): Either[Set[String], Map[String, NativeReducer[Nat]]] = {
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
  def apply(values: (String, NativeReducer[Nat])*): KernelMod = MergeKernelMod(values.toMap)
}
