package io.github.caeus.elodin.value.generic

import io.github.caeus.elodin.value.{Eson, ToEson}

import scala.language.experimental.macros
trait TED
//trait ToEsonDerivation {
//  import magnolia._
//  type Typeclass[T] = ToEson[T]
//  def combine[T](caseClass: CaseClass[Typeclass, T]): Typeclass[T] = { (t: T) =>
//    Eson.DictVal(caseClass.parameters.map { param =>
//      param.label -> param.typeclass.apply(param.dereference(t))
//    }.toMap)
//  }
//  implicit def genTo[T]: Typeclass[T] = macro Magnolia.gen[T]
//}
