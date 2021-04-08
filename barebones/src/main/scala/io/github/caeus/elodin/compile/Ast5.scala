package io.github.caeus.elodin.compile

import io.github.caeus.elodin.util.Pos2Loc
import io.github.caeus.elodin.util.Pos2Loc.Loc
import io.github.caeus.elodin.value.{Eson, Value}

sealed trait Ast5
object Ast5 {
  sealed trait Expr extends Ast5 {
    def loc: Loc
  }

  sealed trait NonApplyExpr extends Expr
  sealed trait NonFunExpr   extends Expr
  object Expr {
    final case class ExtendExpr(bindings: Expr, body: Expr, loc: Loc)
        extends NonApplyExpr
        with NonFunExpr
    final case class ReqExpr(module: String, loc: Loc)    extends NonApplyExpr with NonFunExpr
    final case class RefExpr(to: String, loc: Loc)        extends NonApplyExpr with NonFunExpr
    final case class ListExpr(items: Seq[Expr], loc: Loc) extends NonApplyExpr with NonFunExpr
    final case class DictExpr(items: Map[String, Expr], loc: Loc)
        extends NonApplyExpr
        with NonFunExpr
    final case class ApplyExpr(fn: NonApplyExpr, arg0: Expr, args: Seq[Expr], loc: Loc)
        extends NonFunExpr
    final case class FunExpr(param0: String, params: Seq[String], body: NonFunExpr, loc: Loc)
        extends NonApplyExpr
    final case class RecLetExpr(bindings: Map[String, FunExpr], body: Expr, loc: Loc)
        extends NonApplyExpr
        with NonFunExpr
    final case class EsonExpr(value: Eson, loc: Loc)      extends NonApplyExpr with NonFunExpr
    final case class HostRefExpr(named: String, loc: Loc) extends NonApplyExpr with NonFunExpr
  }

}
