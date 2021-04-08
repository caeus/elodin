package io.github.caeus.elodin.eni

import io.github.caeus.elodin.util.LilThrowable

object StdENIMod extends ENIMod {
  override def value: Map[String, HostFhunk] = {
    ENIMod.merge(
      MathENIMod,
      BoolENIMod,
      ListENIMod,
      TextENIMod,
      FunENIMod
    ) match {
      case Left(value)  => throw new LilThrowable(s"Conflicting names in native reducers: $value")
      case Right(value) => value
    }
  }
}
