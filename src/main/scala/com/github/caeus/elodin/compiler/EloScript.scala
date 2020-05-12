package com.github.caeus.elodin.compiler

sealed trait EloScript {
  def namespace: String
}
object EloScript {
  case class StrScript(namespace: String, code: String) extends EloScript
}
