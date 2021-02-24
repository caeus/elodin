package io.github.caeus.felurian.compile

import io.github.caeus.felurian.value.Value

sealed trait Ast0

object Ast0 {
  sealed trait Expr extends Ast0 {}
  object Expr {
    final case class ListExpr(items: Seq[Expr])                               extends Expr
    final case class DictExpr(items: Map[String, Expr])                       extends Expr
    final case class ReqExpr(module: String)                                  extends Expr
    final case class BlockExpr(statements: Seq[BlockStatement], last: Expr)   extends Expr
    final case class FunExpr(param0: String, params: Seq[String], body: Expr) extends Expr
    final case class ForExpr(statements: Seq[ForStatement], last: Expr)       extends Expr
    final case class ValueExpr(value: Value)                                  extends Expr
    final case class RefExpr(to: String)                                      extends Expr
    final case class ApplyExpr(fn: Expr, arg0: Expr, args: Seq[Expr])         extends Expr

  }
  sealed trait ForStatement {}
  object ForStatement {
    final case class LetBangStatement(binding: String, expr: Expr)  extends ForStatement
    final case class DoBangStatement(expr: Expr)                    extends ForStatement
    final case class BlockStatement(statement: Ast0.BlockStatement) extends ForStatement
  }
  sealed trait BlockStatement {
    final def toFor: ForStatement = Ast0.ForStatement.BlockStatement(this)
  }
  object BlockStatement {
    final case class ImportStatement(elem: Expr)               extends BlockStatement {}
    final case class LetStatement(binding: String, body: Expr) extends BlockStatement {}
    final case class DoStatement(body: Expr)                   extends BlockStatement {}
    final case class FunStatement(
        binding: String,
        param: String,
        params: Seq[String],
        body: Expr
    ) extends BlockStatement
  }
}
