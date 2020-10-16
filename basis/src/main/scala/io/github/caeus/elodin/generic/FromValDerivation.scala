package io.github.caeus.elodin.generic

import io.github.caeus.elodin.core.{FromVal, Val}

import scala.language.experimental.macros

trait FromValDerivation {
  import magnolia._
  type Typeclass[T] = FromVal[T]
  def combine[T](caseClass: CaseClass[Typeclass, T]): Typeclass[T] = {
    case Val.DictS(items) =>
      caseClass
        .constructEither { p =>
          items
            .get(p.label)
            .map { sval =>
              p.typeclass.accept(sval)
            }
            .getOrElse(Left(List(s"Value doesn't contain the property ${p.label}")))
        }
        .left
        .map(_.flatten)
    case p => Left(List(s"Cannot construct a ${caseClass.typeName} from $p"))
  }
  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]
}
