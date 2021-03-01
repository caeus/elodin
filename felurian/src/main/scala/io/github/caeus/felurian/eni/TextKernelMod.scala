package io.github.caeus.felurian.eni

object TextKernelMod extends KernelMod {
  override def value: Map[String, Fhunk] =
    Map(
      "++" -> Fhunk.make { (s0: String, s1: String) =>
        s0 ++ s1
      },
      "charAt" -> Fhunk.make { (s: String, pos: BigInt) =>
        s.charAt(pos.toInt).toString
      }
    )
}
