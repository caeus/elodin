package io.github.caeus.felurian.runtime

sealed abstract class EvalException(msg: String) extends Throwable(msg, null, true, false) {}
object EvalException {
  final case class Undefined(msg: String)     extends EvalException(msg)
  sealed abstract class Error(msg: String)    extends EvalException(msg)
  final case class ReferenceError(to: String) extends Error(s"`$to` is not defined")
  case object WildcardError                   extends Error(s"only dictionaries can be wildcard imported")
  case object ApplyError                      extends Error(s"trying to apply to non function value")

}
