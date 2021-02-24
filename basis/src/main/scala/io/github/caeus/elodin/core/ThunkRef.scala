package io.github.caeus.elodin.core

sealed trait ThunkRef {
  def module: String
  def id: String
  def isForeign: Boolean
}
object ThunkRef {
  final case class Foreign(module: String, id: String) extends ThunkRef {
    override def isForeign: Boolean = true
  }
  final case class Native(module: String, id: String) extends ThunkRef {
    override def isForeign: Boolean = false
  }
}
