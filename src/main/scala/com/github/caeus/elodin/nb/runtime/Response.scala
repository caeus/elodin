package com.github.caeus.elodin.nb.runtime

import com.github.caeus.elodin.nb.archive.{ActionRef, CalculationRef}
import com.github.caeus.elodin.nb.runtime.Value.{Applicable, Atomic}
import io.circe.Json

sealed trait Response {}
object Response {

  case class Job(description: Json, onSuccess: Applicable, onFailure: Applicable) extends Response

  case class Result(value: Either[Value, Value]) extends Response

}
sealed trait Effect
case class EffOp(ref: ActionRef, args: Seq[Value])
object Effect {
  case class Suspend(
      op: EffOp,
      onSuccess: Applicable,
      onFailure: Applicable
  ) extends Effect
  case class Done(value: Either[Value, Value]) extends Effect
}
sealed trait Generator
object Generator {
  case class Suspend(step: Atomic, cont: Applicable) extends Generator
  case class Done(result: Atomic)                     extends Generator
}
