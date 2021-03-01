package io.github.caeus.felurian.compile

sealed trait CompiledModule {
  def name: String
  def ast: Ast9
}

object CompiledModule {
  def make(name: String, ast: Ast9): CompiledModule =
    Impl(name: String, ast)
  private final case class Impl(name: String, ast: Ast9) extends CompiledModule
}
