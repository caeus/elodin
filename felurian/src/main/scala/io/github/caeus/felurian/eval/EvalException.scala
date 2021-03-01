package io.github.caeus.felurian.eval

import io.github.caeus.felurian.value.Value
import zio.ZIO

sealed trait ExceptionKind
object ExceptionKind {
  case object Undefined extends ExceptionKind
  case object Error     extends ExceptionKind
}
sealed trait TraceElement
object TraceElement {
  final case class CodeLocation() extends TraceElement
  final case class NoLocation()   extends TraceElement
}

final case class EvalException(
    msg: String,
    kind: ExceptionKind,
    trace: List[TraceElement],
    cause: Option[EvalException]
) extends Throwable(msg, null, true, false) {
  def prependTrace(loc: TraceElement): ZIO[Resolver, EvalException, Nothing] = {
    ZIO.fail(EvalException(msg, kind, loc :: trace, cause))
  }

  override def toString: String = render
  def render: String = {
    s"""$kind: $msg
       |trace: ${trace.mkString("\t\n")}
       |""".stripMargin
  }

}
object EvalException {

  object Undefined {
    def unapply(evalException: EvalException): Option[EvalException] = {
      if (evalException.kind == ExceptionKind.Undefined) {
        Some(evalException)
      } else None
    }
  }

  def undefined(msg: String): ZIO[Any, EvalException, Nothing] = {
    ZIO.fail(
      EvalException(s"undefined: $msg", ExceptionKind.Undefined, Nil, None)
    )
  }

  def referenceError(to: String): ZIO[Any, EvalException, Nothing] = {
    ZIO.fail(
      EvalException(s"reference to $to is not defined", ExceptionKind.Error, Nil, None)
    )
  }

  def wildcardError(wrongScope: Value): ZIO[Any, EvalException, Nothing] =
    ZIO.fail(
      EvalException(
        s"trying to extend scope with ${Value.typeOf(wrongScope)} ",
        ExceptionKind.Error,
        Nil,
        None
      )
    )

  def applyError(wrongFn: Value): ZIO[Any, EvalException, Nothing] =
    ZIO.fail(
      EvalException(
        s"trying to apply arguments to ${Value.typeOf(wrongFn)} ",
        ExceptionKind.Undefined,
        Nil,
        None
      )
    )

}
