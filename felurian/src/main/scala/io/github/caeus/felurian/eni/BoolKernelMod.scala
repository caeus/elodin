package io.github.caeus.felurian.eni

object BoolKernelMod extends KernelMod {
  def op(f: (Boolean, Boolean) => Boolean) = {
    Fhunk.make(f)
  }
  override def value: Map[String, Fhunk] =
    Map(
      "&&"  -> op(_ && _),
      "||"  -> op(_ || _),
      "not" -> Fhunk.make((b: Boolean) => b.unary_!)
    )

}
