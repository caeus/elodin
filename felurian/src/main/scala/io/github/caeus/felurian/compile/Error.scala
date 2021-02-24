package io.github.caeus.felurian.compile

sealed trait Error {}

object Error{
  final case class Wha(x:Throwable) extends Error
}
