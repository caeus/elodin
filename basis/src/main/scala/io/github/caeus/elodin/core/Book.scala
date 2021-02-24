package io.github.caeus.elodin.core

final case class MemberName(value: String)     extends AnyVal
final case class ModuleThunkRef(value: String) extends AnyVal
trait Export {
  def members: Set[MemberName]
  def page(title: MemberName): Option[ModuleThunkRef]
}

trait Thunks {
  def at(page: ModuleThunkRef): Option[Thunk]
}

/**
  * Qué jodido estrés. Mi bloqueo no es otra cosa que buscar la palabra correcta.
  * @param export
  * @param thunks
  */

final case class Module(export: Export, thunks: Thunks)
