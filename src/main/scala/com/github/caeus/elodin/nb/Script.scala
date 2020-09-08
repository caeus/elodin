package com.github.caeus.elodin.nb

sealed trait Script {
  def namespace: String
}
object Script {
  case class StrScript(namespace: String, code: String) extends Script
}
