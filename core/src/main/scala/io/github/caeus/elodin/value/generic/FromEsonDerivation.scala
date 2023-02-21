package io.github.caeus.elodin.value.generic

import io.github.caeus.elodin.value.{Eson, FromEson, generic}

import scala.language.experimental.macros
import magnolia.*
object FromEsonDerivation extends AutoDerivation[FromEson]{
  override def join[T](ctx: CaseClass[FromEson, T]): generic.FromEsonDerivation.Typeclass[T] = {
    case Eson.DictVal(items) =>
      ctx
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
    case p => Left(List(s"Cannot construct a ${ctx.typeInfo.full} from $p"))
  }

  override def split[T](ctx: SealedTrait[FromEson, T]) = ???

}
