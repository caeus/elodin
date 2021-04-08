package io.github.caeus.elodin.eni

object BoolENIMod extends ENIMod {
  def op(f: (Boolean, Boolean) => Boolean) = {
    HostFhunk.make(f)
  }
  override def value: Map[String, HostFhunk] =
    Map(
      "(&&)"  -> op(_ && _),
      "(||)"  -> op(_ || _),
      "not" -> HostFhunk.make((b: Boolean) => b.unary_!)
    )

}
