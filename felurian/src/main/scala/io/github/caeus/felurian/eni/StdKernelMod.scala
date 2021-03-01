package io.github.caeus.felurian.eni

import io.github.caeus.felurian.util.LilThrowable

object StdKernelMod extends KernelMod {
  override def value: Map[String, Fhunk] = {
    KernelMod.merge(
      MathKernelMod,
      BoolKernelMod,
      ListKernelMod,
      TextKernelMod,
      FunKernelMod
    ) match {
      case Left(value)  => throw new LilThrowable(s"Conflicting names in native reducers: $value")
      case Right(value) => value
    }
  }
}
