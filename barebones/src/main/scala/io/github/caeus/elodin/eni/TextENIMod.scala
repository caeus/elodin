package io.github.caeus.elodin.eni

object TextENIMod extends ENIMod {
  override def value: Map[String, HostFhunk] =
    Map(
      "(++)" -> HostFhunk.make { (s0: String, s1: String) =>
        s0 ++ s1
      },
      "charAt" -> HostFhunk.make { (s: String, pos: BigInt) =>
        s.charAt(pos.toInt).toString
      }
    )
}
