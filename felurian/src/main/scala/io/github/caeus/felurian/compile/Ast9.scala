package io.github.caeus.felurian.compile

import io.github.caeus.felurian.value.Value

sealed trait Ast9
object Ast9 {
  sealed trait Expr extends Ast9

  sealed trait NonApplyExpr extends Expr
  sealed trait NonFunExpr   extends Expr
  object Expr {
    final case class ExtendExpr(bindings: Expr, body: Expr)                   extends NonApplyExpr with NonFunExpr
    final case class ReqExpr(module: String)                                  extends NonApplyExpr with NonFunExpr
    final case class RefExpr(to: String)                                      extends NonApplyExpr with NonFunExpr
    final case class ListExpr(items: Seq[Expr])                               extends NonApplyExpr with NonFunExpr
    final case class DictExpr(items: Map[String, Expr])                       extends NonApplyExpr with NonFunExpr
    final case class ApplyExpr(fn: NonApplyExpr, arg0: Expr, args: Seq[Expr]) extends NonFunExpr
    final case class FunExpr(param0: String, params: Seq[String], body: NonFunExpr)
        extends NonApplyExpr
    final case class RecLetExpr(bindings: Map[String, FunExpr], body: Expr)
        extends NonApplyExpr
        with NonFunExpr
    final case class ValueExpr(value: Value)    extends NonApplyExpr with NonFunExpr
    final case class ForeignExpr(named: String) extends NonApplyExpr with NonFunExpr
  }

}
