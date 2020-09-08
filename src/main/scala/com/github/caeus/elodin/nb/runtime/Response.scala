package com.github.caeus.elodin.nb.runtime

import com.github.caeus.elodin.nb.archive.{ActionRef, CalculationRef}
import com.github.caeus.elodin.nb.runtime.Value.Applicable
import io.circe.Json

sealed trait Response {}
object Response {

  case class Job(description: Json, onSuccess: Applicable, onFailure: Applicable) extends Response

  case class Result(value: Either[Value, Value]) extends Response

}
sealed trait Effect
object Effect {
  case class Suspend(
      page: ActionRef,
      args: Seq[Value],
      onSuccess: Applicable,
      onFailure: Applicable
  ) extends Effect
  case class Result(value: Either[Value, Value]) extends Effect
}
