package io.github.caeus.felurian

import io.circe.Json
import zio.IO

sealed trait Workflow {}
sealed trait WorkflowState
object WorkflowState {
  final case class Suspended() extends WorkflowState
}
trait TaskNovelties {}
//This is for workflows, stuff that can be run and then suspended...
//My idea of how this is gonna work is still blurry
final class WorkflowRunner {
  def compile(namespace: String, code: String): IO[Throwable, Workflow] = ???
  def run(
      workflow: Workflow,
      input: Json
  ): IO[Throwable, WorkflowState] = ???
  def continue(
      state: WorkflowState,
      novelties: TaskNovelties
  ): IO[Throwable, WorkflowState] = ???

}
