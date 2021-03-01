package io.github.caeus.felurian.eval

import io.github.caeus.felurian.value.Value
import io.github.caeus.felurian.compile.{Ast9, CompiledModule}

sealed trait EvaluatedModule {
  def name: String
  def ast: Ast9
  def value: Value
}
object EvaluatedModule {
  def make(compiledModule: CompiledModule, value: Value): EvaluatedModule =
    Impl(compiledModule.name, compiledModule.ast, value)
  private final case class Impl(name: String, ast: Ast9, value: Value) extends EvaluatedModule
}
