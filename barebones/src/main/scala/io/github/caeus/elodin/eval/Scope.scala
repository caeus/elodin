package io.github.caeus.elodin.eval

import io.github.caeus.elodin.compile.Ast9.FhunkRef
import io.github.caeus.elodin.value.Value
import io.github.caeus.elodin.value.Value.ImplRef

final class Scope(val module: String, underlying: Map[String, Either[FhunkRef.Guest, Value]]) {
  def extendWithFhunkRefs(upon: Map[String, FhunkRef.Guest]): Scope = {
    new Scope(module, underlying ++ upon.view.mapValues(v => Left(v)).toMap)

  }

  def extendWithValues(upon: Map[String, Value]): Scope = {
    new Scope(module, underlying ++ upon.view.mapValues(v => Right(v)).toMap)
  }

  def get(name: String): Option[Value] = {
    underlying.get(name).map {
      case Right(v) => v
      case Left(ref: FhunkRef.Guest) =>
        Value.FunVal(ImplRef.Guest(ref.module, ref.index, this), Nil)
    }
  }

}
object Scope {
  def fromMap(module: String, bindings: Map[String, Value]): Scope = {
    new Scope(module, bindings.view.mapValues(v => Right(v)).toMap)
  }
}
