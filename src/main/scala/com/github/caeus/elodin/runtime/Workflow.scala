package com.github.caeus.elodin.runtime

import com.github.caeus.elodin.archive.ActionRef
import com.github.caeus.elodin.runtime.Value.{Applicable, Atomic}
import io.circe.Json

sealed trait Workflow {}
object Workflow {

  case class Job(description: Json, onSuccess: Applicable, onFailure: Applicable) extends Workflow

  case class Done(value: Either[Value, Value]) extends Workflow

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
