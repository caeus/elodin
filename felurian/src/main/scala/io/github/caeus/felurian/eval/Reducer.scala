package io.github.caeus.felurian.eval

import io.github.caeus.felurian.compile.Ast9
import io.github.caeus.felurian.compile.Ast9.Expr.{ExtendExpr, ValueExpr}
import io.github.caeus.felurian.eval.Ctx.root
import io.github.caeus.felurian.value.Value
import io.github.caeus.felurian.value.Value.DictVal
import zio.ZIO

trait Reducer {
  def run(ast9: Ast9, rootBindings: Map[String, Value]): ZIO[Resolver, EvalException, Value]
}
final class LiveReducer extends Reducer {

  def runExpr(expr: Ast9.Expr): ZIO[Resolver, EvalException, Value] = {
    root(expr).eval
  }

  override def run(
      ast9: Ast9,
      rootBindings: Map[String, Value]
  ): ZIO[Resolver, EvalException, Value] = {
    ast9 match {
      case expr: Ast9.Expr => runExpr(ExtendExpr(ValueExpr(DictVal(rootBindings)), expr))
    }
  }
}
