package io.github.caeus.elodin.generic

import io.github.caeus.elodin.core.{ToVal, Val}

import scala.language.experimental.macros

trait ToValDerivation {
  import magnolia._
  type Typeclass[T] = ToVal[T]
  def combine[T](caseClass: CaseClass[Typeclass, T]): Typeclass[T] = { (t: T) =>
    {
      Val.DictS(caseClass.parameters.map { p =>
        p.label -> p.typeclass.cast(p.dereference(t))
      }.toMap)
    }
  }
  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]
}
