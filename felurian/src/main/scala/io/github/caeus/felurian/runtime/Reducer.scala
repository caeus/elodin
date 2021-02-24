package io.github.caeus.felurian.runtime

import io.github.caeus.felurian.compile.Ast9
import io.github.caeus.felurian.compile.Ast9.Expr.{SpreadExpr, ValueExpr}
import io.github.caeus.felurian.runtime.Ctx.root
import io.github.caeus.felurian.value.Value
import io.github.caeus.felurian.value.Value.DictVal

trait Reducer {
  def run(ast9: Ast9, rootBindings: Map[String, Value]): Ctx.EvalVal
}
final class LiveReducer extends Reducer {

  def runExpr(expr: Ast9.Expr): Ctx.EvalVal = {
    root(expr).eval
  }

  override def run(ast9: Ast9, rootBindings: Map[String, Value]): Ctx.EvalVal = {
    ast9 match {
      case expr: Ast9.Expr => runExpr(SpreadExpr(ValueExpr(DictVal(rootBindings)), expr))
    }
  }
}
