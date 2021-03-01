package io.github.caeus.felurian.value.generic

import io.github.caeus.felurian.value.{Eson, FromEson}
import scala.language.experimental.macros
trait FromEsonDerivation {
  import magnolia._
  type Typeclass[T] = FromEson[T]
  def combine[T](caseClass: CaseClass[Typeclass, T]): Typeclass[T] = {
    case Eson.DictVal(items) =>
      caseClass
        .constructEither { p =>
          items
            .get(p.label)
            .map { sval =>
              p.typeclass(sval)
            }
            .getOrElse(Left(List(s"Value doesn't contain the property ${p.label}")))
        }
        .left
        .map(_.flatten)
    case p => Left(List(s"Cannot construct a ${caseClass.typeName} from $p"))
  }
  implicit def genFrom[T]: Typeclass[T] = macro Magnolia.gen[T]
}
