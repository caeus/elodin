package io.github.caeus.felurian.eval

import io.github.caeus.felurian.compile.{Ast9, LiveCompiler}
import io.github.caeus.felurian.eval.Ctx.RefCtx
import io.github.caeus.felurian.value.Value
import zio.{IO, Task, ZIO, ZLayer}

trait Evaluator {
  def compile(name: String, code: String): Task[Unit]
  def module(module: String): ZIO[Any, EvalException, Value]
  def resolve[E, X](zio: ZIO[Resolver, E, X]): IO[E, X]
}

object Evaluator {
  def make(moduleRegistry: ModuleRegistry, eni: ENI): Evaluator = {
    new LiveEvaluator(moduleRegistry, eni): Evaluator
  }
  val live =
    ZLayer.fromServices(make _)
}
final class LiveEvaluator(moduleRegistry: ModuleRegistry, eni: ENI) extends Evaluator {
  private val compiler = new LiveCompiler
  private val reducer  = new LiveReducer
  private val resolver = Resolver.make(this, eni)
  private def rootBindings(name: String): ZIO[Resolver, EvalException, Map[String, Value]] = {
    eni.rootBindings
  }
  override def compile(name: String, code: String): Task[Unit] = {
    (for {

      compiledModule <- compiler.compile(name, code)
      rootBindings   <- rootBindings(name)
      value          <- reducer.run(compiledModule.ast, rootBindings)
      _              <- moduleRegistry.register(EvaluatedModule.make(compiledModule, value))
    } yield ()).provide(resolver)
  }

  override def module(module: String): ZIO[Any, EvalException, Value] = {
    moduleRegistry
      .byName(module)
      .map(_.map(_.value))
      .someOrElseM(EvalException.referenceError(s"module:`$module`"))
  }

  override def resolve[E, X](zio: ZIO[Resolver, E, X]): IO[E, X] =
    zio.provide(resolver)
}
