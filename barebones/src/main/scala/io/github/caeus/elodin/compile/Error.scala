package io.github.caeus.elodin.compile

sealed trait Error {}

object Error{
  final case class Wha(x:Throwable) extends Error
}
