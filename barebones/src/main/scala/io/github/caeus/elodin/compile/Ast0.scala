package io.github.caeus.elodin.compile

import io.github.caeus.elodin.util.Pos2Loc
import io.github.caeus.elodin.util.Pos2Loc.Loc
import io.github.caeus.elodin.value.Eson

sealed trait Ast0

object Ast0 {
  sealed trait Expr extends Ast0 {
    def loc: Pos2Loc.Loc
  }
  object Expr {
    final case class ListExpr(items: Seq[Expr], loc: Pos2Loc.Loc)         extends Expr
    final case class DictExpr(items: Map[String, Expr], loc: Pos2Loc.Loc) extends Expr
    final case class ReqExpr(module: String, loc: Pos2Loc.Loc)            extends Expr
    final case class BlockExpr(statements: Seq[BlockStatement], last: Expr, loc: Pos2Loc.Loc)
        extends Expr
    final case class FunExpr(param0: String, params: Seq[String], body: Expr, loc: Pos2Loc.Loc)
        extends Expr
    final case class ForExpr(statements: Seq[ForStatement], last: Expr, loc: Pos2Loc.Loc)
        extends Expr
    final case class EsonExpr(value: Eson, loc: Pos2Loc.Loc)                            extends Expr
    final case class RefExpr(to: String, loc: Pos2Loc.Loc)                              extends Expr
    final case class ApplyExpr(fn: Expr, arg0: Expr, args: Seq[Expr], loc: Pos2Loc.Loc) extends Expr

  }
  sealed trait ForStatement {
    def loc: Pos2Loc.Loc
  }
  object ForStatement {
    final case class LetBangStatement(binding: String, expr: Expr, loc: Pos2Loc.Loc)
        extends ForStatement
    final case class DoBangStatement(expr: Expr, loc: Pos2Loc.Loc) extends ForStatement
    final case class BlockStatement(statement: Ast0.BlockStatement) extends ForStatement {
      override def loc: Loc = statement.loc
    }
  }
  sealed trait BlockStatement {
    def loc: Pos2Loc.Loc
    final def toFor: ForStatement = Ast0.ForStatement.BlockStatement(this)
  }
  object BlockStatement {
    final case class ImportStatement(elem: Expr, loc: Loc)               extends BlockStatement {}
    final case class LetStatement(binding: String, body: Expr, loc: Loc) extends BlockStatement {}
    final case class DoStatement(body: Expr, loc: Loc)                   extends BlockStatement {}
    final case class FunStatement(
        binding: String,
        param: String,
        params: Seq[String],
        body: Expr,
        loc: Loc
    ) extends BlockStatement
  }
}
