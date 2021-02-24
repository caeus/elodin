package io.github.caeus.felurian.runtime

import io.github.caeus.felurian.compile.Ast9
import io.github.caeus.felurian.compile.Ast9.Expr
import io.github.caeus.felurian.compile.Ast9.Expr._
import io.github.caeus.felurian.value.Value
import io.github.caeus.felurian.value.Value.FunVal
import zio.ZIO

sealed abstract class Ctx[+E <: Expr](val parent: Option[Ctx[Expr]], val expr: E) {
  def resolve(to: String): Ctx.EvalVal =
    parent
      .map(_.resolve(to))
      .getOrElse(ZIO.fail(EvalException.ReferenceError(to)))

  def eval: Ctx.EvalVal
}
object Ctx {
  type Eval[X] = ZIO[ModuleSystem, EvalException, X]
  type EvalVal = Eval[Value]

  def make(parent: Option[Ctx[Expr]], expr: Expr): Ctx[Expr] = {

    expr match {
      case expr: ListExpr    => new ListCtx(parent, expr)
      case expr: DictExpr    => new DictCtx(parent, expr)
      case expr: SpreadExpr  => new SpreadCtx(parent, expr)
      case expr: ReqExpr     => new ReqCtx(parent, expr)
      case expr: RefExpr     => new RefCtx(parent, expr)
      case expr: FunExpr     => new FunCtx(parent, expr)
      case expr: ApplyExpr   => new ApplyCtx(parent, expr)
      case expr: RecLetExpr  => new RecLetCtx(parent, expr)
      case expr: ValueExpr   => new ValueCtx(parent, expr)
      case expr: ForeignExpr => new ForeignCtx(parent, expr)
    }
  }
  def root(expr: Ast9.Expr): Ctx[Expr] = {
    make(None, expr)
  }
  def leaf(parent: Ctx[Expr], expr: Ast9.Expr): Ctx[Expr] = {
    make(Some(parent), expr)
  }

  final class ListCtx(parent: Option[Ctx[Expr]], expr: ListExpr) extends Ctx(parent, expr) {
    override def eval: EvalVal = {
      ZIO
        .collectAll(expr.items.map { item =>
          leaf(this, item).eval
        })
        .map(v => Value.ListVal(v))
    }
  }

  final class DictCtx(parent: Option[Ctx[Expr]], expr: DictExpr) extends Ctx(parent, expr) {
    override def eval: EvalVal = {
      ZIO
        .collectAll(expr.items.map {
          case (k, v) => leaf(this, v).eval.map(k -> _)
        })
        .map { v =>
          Value.DictVal(v.toMap)
        }
    }
  }

  final class SpreadCtx(parent: Option[Ctx[Expr]], expr: SpreadExpr) extends Ctx(parent, expr) {
    override def eval: EvalVal = {
      leaf(this, expr.bindings).eval.flatMap {
        case Value.DictVal(scope) =>
          new SpreadBodyCtx(this, scope, expr.body).eval
        case _ => ZIO.fail(EvalException.WildcardError)
      }
    }

    override def resolve(to: String): EvalVal =
      parent
        .map(_.resolve(to))
        .getOrElse(ZIO.fail(EvalException.ReferenceError(to)))
  }
  final class SpreadBodyCtx(parent: SpreadCtx, bindings: Map[String, Value], expr: Expr)
      extends Ctx(Some(parent), expr) {
    override def eval: EvalVal = leaf(this, expr).eval

    override def resolve(to: String): EvalVal =
      bindings
        .get(to)
        .map(v => ZIO.succeed(v))
        .getOrElse(parent.resolve(to))
  }
  final class ReqCtx(parent: Option[Ctx[Expr]], expr: ReqExpr) extends Ctx(parent, expr) {
    override def eval: EvalVal =
      ZIO.environment[ModuleSystem].flatMap { system =>
        system.module(expr.module)
      }
  }
  final class RefCtx(parent: Option[Ctx[Expr]], expr: RefExpr) extends Ctx(parent, expr) {
    override def eval: EvalVal =
      ZIO
        .succeed(parent)
        .someOrFail(EvalException.ReferenceError(expr.to))
        .flatMap { parent =>
          parent.resolve(expr.to)
        }
  }
  final class ApplyCtx(parent: Option[Ctx[Expr]], expr: ApplyExpr) extends Ctx(parent, expr) {
    override def eval: EvalVal = {
      ZIO
        .mapN(
          leaf(this, expr.fn).eval,
          leaf(this, expr.arg0).eval,
          ZIO.collectAll(
            expr.args
              .map(leaf(this, _).eval)
          )
        ) { (fn, arg0, args) =>
          fn.applicable(arg0, args)
        }
        .flatten
    }
  }
  final class FunCtx(parent: Option[Ctx[Expr]], expr: FunExpr) extends Ctx[FunExpr](parent, expr) {
    override def eval: EvalVal    = ZIO.succeed(FunVal(this, Nil))
    private lazy val joinedParams = expr.params.prepended(expr.param0)
    lazy val arity: Int           = joinedParams.size
    def reduce(args: Seq[Value]): Ctx[Expr] = {
      new FunBodyCtx(this, joinedParams.zip(args).toMap, expr.body)
    }
  }
  final class FunBodyCtx(parent: FunCtx, bindings: Map[String, Value], expr: Expr)
      extends Ctx(Some(parent), expr) {
    override def eval: EvalVal = leaf(this, expr).eval

    override def resolve(to: String): EvalVal =
      bindings
        .get(to)
        .map(v => ZIO.succeed(v))
        .getOrElse(parent.resolve(to))
  }
  final class RecLetCtx(parent: Option[Ctx[Expr]], expr: RecLetExpr) extends Ctx(parent, expr) {
    override def eval: EvalVal = leaf(this, expr.body).eval

    override def resolve(to: String): EvalVal =
      expr.bindings
        .get(to)
        .map { expr =>
          leaf(this, expr).eval
        }
        .orElse(parent.map(_.resolve(to)))
        .getOrElse(ZIO.fail(EvalException.ReferenceError(to)))
  }
  final class ValueCtx(parent: Option[Ctx[Expr]], expr: ValueExpr) extends Ctx(parent, expr) {
    override def eval: EvalVal = ZIO.succeed(expr.value)
  }
  final class ForeignCtx(parent: Option[Ctx[Expr]], expr: ForeignExpr) extends Ctx(parent, expr) {
    override def eval: EvalVal = ZIO.accessM[ModuleSystem](_.foreignValue(expr.named))
  }

}
