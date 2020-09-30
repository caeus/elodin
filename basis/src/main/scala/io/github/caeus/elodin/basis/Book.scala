package io.github.caeus.elodin.basis

trait Book {
  def title: String
  def exported: Set[String]
  def thunk(id: String): Option[Thunk]
}
