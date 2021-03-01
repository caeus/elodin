package io.github.caeus.felurian.eval

import io.github.caeus.felurian.compile.Ast9
import io.github.caeus.felurian.compile.Ast9.Expr
import io.github.caeus.felurian.compile.Ast9.Expr._
import io.github.caeus.felurian.eval.TraceElement.CodeLocation
import io.github.caeus.felurian.value.Value
import io.github.caeus.felurian.value.Value.{Applicable, FunVal}
import zio.ZIO

final case class Up(ctx: Ctx[Expr], segment: String)

sealed abstract class Ctx[+E <: Expr](val up: Option[Up], val expr: E) {
  def loc: CodeLocation = CodeLocation()
  def resolve(to: String): ZIO[Resolver, EvalException, Value] =
    up.map(_.ctx.resolve(to))
      .getOrElse(EvalException.referenceError(to))

  def eval: ZIO[Resolver, EvalException, Value]
}
object Ctx {
  //FFI ? not sure, this element here should have two main functions
  //1. Yes, resolve foreign functions AND
  //2. Resolve modules
  type Eval[X] = ZIO[Resolver, EvalException, X]
  type EvalVal = Eval[Value]

  def noop(name: String) = new RefCtx(None, Ast9.Expr.RefExpr(name))
  def make(up: Option[Up], expr: Expr): Ctx[Expr] = {

    expr match {
      case expr: ListExpr    => new ListCtx(up, expr)
      case expr: DictExpr    => new DictCtx(up, expr)
      case expr: ExtendExpr  => new ExtendCtx(up, expr)
      case expr: ReqExpr     => new ReqCtx(up, expr)
      case expr: RefExpr     => new RefCtx(up, expr)
      case expr: FunExpr     => new FunCtx(up, expr)
      case expr: ApplyExpr   => new ApplyCtx(up, expr)
      case expr: RecLetExpr  => new RecLetCtx(up, expr)
      case expr: ValueExpr   => new ValueCtx(up, expr)
      case expr: ForeignExpr => new ForeignCtx(up, expr)
    }
  }
  def root(expr: Ast9.Expr): Ctx[Expr] = {
    make(None, expr)
  }
  def branch(parent: Ctx[Expr], segment: String, expr: Ast9.Expr): Ctx[Expr] = {
    make(Some(Up(parent, segment)), expr)
  }

  final class ListCtx(up: Option[Up], expr: ListExpr) extends Ctx(up, expr) {
    override def eval: ZIO[Resolver, EvalException, Value] = {
      ZIO
        .collectAll(expr.items.zipWithIndex.map {
          case (item, i) =>
            branch(this, s"litem:$i", item).eval
        })
        .map(v => Value.ListVal(v))
    }
  }

  final class DictCtx(up: Option[Up], expr: DictExpr) extends Ctx(up, expr) {
    override def eval: ZIO[Resolver, EvalException, Value] = {
      ZIO
        .collectAll(expr.items.map {
          case (k, v) => branch(this, s"ditem:${k}", v).eval.map(k -> _)
        })
        .map { v =>
          Value.DictVal(v.toMap)
        }
    }
  }

  final class ExtendCtx(up: Option[Up], expr: ExtendExpr) extends Ctx(up, expr) {
    override def eval: ZIO[Resolver, EvalException, Value] = {
      branch(this, "import:bindings", expr.bindings).eval
        .flatMap {
          case Value.DictVal(scope) =>
            new ExtendBodyCtx(this, scope, expr.body).eval
          case obj =>
            EvalException.wildcardError(obj)
        }
        .catchAll(_.prependTrace(loc))
    }

    override def resolve(to: String): ZIO[Resolver, EvalException, Value] =
      up.map(_.ctx.resolve(to))
        .getOrElse(
          EvalException.referenceError(to)
        )
  }
  final class ExtendBodyCtx(parent: ExtendCtx, bindings: Map[String, Value], expr: Expr)
      extends Ctx(Some(Up(parent, "import:body")), expr) {
    override def eval: ZIO[Resolver, EvalException, Value] = branch(this, "import:body", expr).eval

    override def resolve(to: String): ZIO[Resolver, EvalException, Value] =
      bindings
        .get(to)
        .map(v => ZIO.succeed(v))
        .getOrElse(parent.resolve(to))
  }
  final class ReqCtx(up: Option[Up], expr: ReqExpr) extends Ctx(up, expr) {
    override def eval: ZIO[Resolver, EvalException, Value] =
      ZIO.environment[Resolver].flatMap { system =>
        system.module(expr.module)
      }
  }
  final class RefCtx(up: Option[Up], expr: RefExpr) extends Ctx(up, expr) {
    override def eval: ZIO[Resolver, EvalException, Value] =
      ZIO
        .succeed(up)
        .someOrElseM(
          EvalException.referenceError(expr.to)
        )
        .flatMap { up =>
          up.ctx.resolve(expr.to)
        }
        .catchAll { err =>
          err.prependTrace(loc)
        }
  }
  final class ApplyCtx(up: Option[Up], expr: ApplyExpr) extends Ctx(up, expr) {
    override def eval: ZIO[Resolver, EvalException, Value] = {
      ZIO
        .mapN(
          branch(this, "fun:fn", expr.fn).eval,
          branch(this, "farg:0", expr.arg0).eval,
          ZIO.collectAll(
            expr.args.zipWithIndex
              .map(arg => branch(this, s"farg:${arg._2}", arg._1).eval)
          )
        ) { (fn: Value, arg0, args) =>
          fn match {
            case app: Applicable =>
              app(arg0, args)
            case obj =>
              EvalException.applyError(obj)
          }
        }
        .flatten
        .catchAll(_.prependTrace(loc))

    }
  }
  final class FunCtx(up: Option[Up], expr: FunExpr) extends Ctx[FunExpr](up, expr) {
    override def eval: ZIO[Resolver, EvalException, Value] = ZIO.succeed(FunVal(this, Nil))
    private lazy val joinedParams                          = expr.params.prepended(expr.param0)
    lazy val arity: Int                                    = joinedParams.size
    def reduce(args: Seq[Value]): Ctx[Expr] = {
      new FunBodyCtx(this, joinedParams.zip(args).toMap, expr.body)
    }
  }
  final class FunBodyCtx(parent: FunCtx, bindings: Map[String, Value], expr: Expr)
      extends Ctx(Some(Up(parent, "fun:body")), expr) {
    override def eval: ZIO[Resolver, EvalException, Value] = branch(this, "_", expr).eval

    override def resolve(to: String): ZIO[Resolver, EvalException, Value] =
      bindings
        .get(to)
        .map(v => ZIO.succeed(v))
        .getOrElse(parent.resolve(to))
  }
  final class RecLetCtx(up: Option[Up], expr: RecLetExpr) extends Ctx(up, expr) {
    override def eval: ZIO[Resolver, EvalException, Value] =
      branch(this, "rec:body", expr.body).eval

    override def resolve(to: String): ZIO[Resolver, EvalException, Value] =
      expr.bindings
        .get(to)
        .map { expr =>
          branch(this, s"rec_binding:$to", expr).eval
        }
        .orElse(up.map(_.ctx.resolve(to)))
        .getOrElse(
          EvalException.referenceError(to)
        )
  }
  final class ValueCtx(up: Option[Up], expr: ValueExpr) extends Ctx(up, expr) {
    override def eval: ZIO[Resolver, EvalException, Value] = ZIO.succeed(expr.value)
  }
  final class ForeignCtx(up: Option[Up], expr: ForeignExpr) extends Ctx(up, expr) {
    override def eval: ZIO[Resolver, EvalException, Value] =
      ZIO.accessM[Resolver](
        _.foreignValue(expr.named)
      )
  }

}
