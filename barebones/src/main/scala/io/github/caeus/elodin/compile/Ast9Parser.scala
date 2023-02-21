package io.github.caeus.elodin.compile

import io.github.caeus.elodin.compile.Ast5.Expr
import io.github.caeus.elodin.compile.Ast9.{FhunkRef, GuestFhunk}
import io.github.caeus.elodin.compile.Ast9Parser.Ctx5
import io.github.caeus.elodin.compile.Ast9Parser.Ctx5.FunCtx5
import io.github.caeus.elodin.eval.TraceElement
import zio.{Task, ZIO}

import scala.util.chaining._

trait Ast9Parser {
  def parse(module: String, ast: Ast5): Task[Ast9.GuestModule]
}
object Ast9Parser {
  def make: Ast9Parser = new LiveAst9Parser

  final case class Up(parent: Ctx5[Ast5.Expr], segment: String)
  sealed abstract class Ctx5[+Expr <: Ast5.Expr](up: Option[Up], val expr: Expr) {
    final lazy val path: Vector[String] = up
      .map { up =>
        up.parent.path.appended(up.segment)
      }
      .getOrElse(Vector.empty)
  }
  object Ctx5 {

    def make[Ex <: Expr](up: Option[Up], expr: Ex): Ctx5[Ex] = {
      expr
        .pipe {
          case expr: Expr.ExtendExpr  => new ExtendCtx5(up, expr)
          case expr: Expr.ReqExpr     => new ReqCtx5(up, expr)
          case expr: Expr.RefExpr     => new RefCtx5(up, expr)
          case expr: Expr.ListExpr    => new ListCtx5(up, expr)
          case expr: Expr.DictExpr    => new DictCtx5(up, expr)
          case expr: Expr.FunExpr     => new FunCtx5(up, expr)
          case expr: Expr.ApplyExpr   => new ApplyCtx5(up, expr)
          case expr: Expr.RecLetExpr  => new RecLetCtx5(up, expr)
          case expr: Expr.EsonExpr    => new ValueCtx5(up, expr)
          case expr: Expr.HostRefExpr => new HostRefCtx5(up, expr)
        }
        .asInstanceOf[Ctx5[Ex]]
    }
    def branch[Ex <: Expr](parent: Ctx5[Expr], segment: String, expr: Ex): Ctx5[Ex] = {
      make(Some(Up(parent, segment)), expr)
    }
    def root[Ex <: Expr](expr: Ex): Ctx5[Ex] = make(None, expr)

    final class ExtendCtx5(up: Option[Up], expr: Expr.ExtendExpr)
        extends Ctx5[Expr.ExtendExpr](up, expr) {
      lazy val bindings: Ctx5[Expr] = branch(this, "extend:bindings", expr.bindings)
      lazy val body: Ctx5[Expr]     = branch(this, "extend:body", expr.bindings)
    }

    final class ReqCtx5(up: Option[Up], expr: Expr.ReqExpr) extends Ctx5[Expr.ReqExpr](up, expr) {
      lazy val module = expr.module
    }
    final class RefCtx5(up: Option[Up], expr: Expr.RefExpr) extends Ctx5[Expr.RefExpr](up, expr) {
      lazy val to = expr.to
    }
    final class ListCtx5(up: Option[Up], expr: Expr.ListExpr)
        extends Ctx5[Expr.ListExpr](up, expr) {
      lazy val items = expr.items.zipWithIndex.map {
        case (item, index) => branch(this, s"li:$index", item)
      }
    }
    final class DictCtx5(up: Option[Up], expr: Expr.DictExpr)
        extends Ctx5[Expr.DictExpr](up, expr) {
      lazy val items: Map[String, Ctx5[Expr]] = expr.items.map {
        case (key, item) => key -> branch(this, s"di:$key", item)
      }
    }
    final class FunCtx5(up: Option[Up], expr: Expr.FunExpr) extends Ctx5[Expr.FunExpr](up, expr) {
      lazy val param0: String      = expr.param0
      lazy val params: Seq[String] = expr.params
      lazy val body: Ctx5[Expr]    = branch(this, "fun:body", expr.body)
    }
    final class ApplyCtx5(up: Option[Up], expr: Expr.ApplyExpr)
        extends Ctx5[Expr.ApplyExpr](up, expr) {
      lazy val fn: Ctx5[Expr]   = branch(this, "apply:fn", expr.fn)
      lazy val arg0: Ctx5[Expr] = branch(this, "apply:arg0", expr.arg0)
      lazy val args: Seq[Ctx5[Expr]] = expr.args.zipWithIndex.map {
        case (arg, index) => branch(this, s"apply:arg${index + 1}", arg)
      }
    }
    final class RecLetCtx5(up: Option[Up], expr: Expr.RecLetExpr)
        extends Ctx5[Expr.RecLetExpr](up, expr) {
      lazy val bindings: Map[String, FunCtx5] = expr.bindings.map {
        case (key, binding) => key -> branch(this, s"rec:bind_$key", binding).asInstanceOf[FunCtx5]
      }
      lazy val body = branch(this, "rec:body", expr.body)
    }
    final class ValueCtx5(up: Option[Up], expr: Expr.EsonExpr)
        extends Ctx5[Expr.EsonExpr](up, expr) {
      lazy val value = expr.value
    }
    final class HostRefCtx5(up: Option[Up], expr: Expr.HostRefExpr)
        extends Ctx5[Expr.HostRefExpr](up, expr) {
      lazy val named = expr.named
    }
  }
}
final class LiveAst9Parser extends Ast9Parser {

  def buildGuestFhunk(module: String, index: Map[Vector[String], Int])(
      ctx: Ctx5[Expr]
  ): Ast9.GuestFhunk = {
    ctx match {
      case ctx: Ctx5.FunCtx5 =>
        GuestFhunk(
          ctx.params.prepended(ctx.param0).toVector,
          parseBody(module, index)(ctx.body)
        )
      case _ =>
        GuestFhunk(Vector.empty, parseBody(module, index)(ctx))
    }
  }
  def parseHeader(ctx: FunCtx5): Vector[String] = ctx.params.prepended(ctx.param0).toVector
  def parseBody(module: String, index: Map[Vector[String], Int])(ctx: Ctx5[Expr]): Ast9.Expr = {
    def cont(ctx: Ctx5[Expr]): Ast9.Expr = parseBody(module, index)(ctx)
    ctx match {
      case ctx: Ctx5.ExtendCtx5 =>
        Ast9.Expr.Extend(cont(ctx.bindings), cont(ctx.body), ctx.expr.loc)
      case ctx: Ctx5.ReqCtx5 =>
        Ast9.Expr.Req(ctx.module, ctx.expr.loc)
      case ctx: Ctx5.RefCtx5 =>
        Ast9.Expr.Ref(ctx.to, ctx.expr.loc)
      case ctx: Ctx5.ListCtx5 =>
        Ast9.Expr.List(
          ctx.items.map { item =>
            cont(item)
          },
          ctx.expr.loc
        )
      case ctx: Ctx5.DictCtx5 =>
        Ast9.Expr.Dict(
          ctx.items.view.mapValues { item =>
            cont(item)
          }.toMap,
          ctx.expr.loc
        )
      case ctx: Ctx5.FunCtx5 =>
        Ast9.Expr.Fun(index(ctx.path), ctx.expr.loc)
      case ctx: Ctx5.ApplyCtx5 =>
        Ast9.Expr.Apply(cont(ctx.fn), cont(ctx.arg0), ctx.args.map(cont), ctx.expr.loc)
      case ctx: Ctx5.RecLetCtx5 =>
        Ast9.Expr.Rec(
          ctx.bindings.map {
            case (key, binding) => key -> FhunkRef.Guest(module, index(binding.path))
          },
          cont(ctx.body),
          ctx.expr.loc
        )
      case ctx: Ctx5.ValueCtx5 =>
        Ast9.Expr.Literal(ctx.value, ctx.expr.loc)
      case ctx: Ctx5.HostRefCtx5 =>
        Ast9.Expr.HostRef(ctx.named, ctx.expr.loc)
    }
  }

  def parse(module: String, ctxs: Vector[Ctx5[Expr]]): Ast9.GuestModule = {
    val index = ctxs.zipWithIndex.map {
      case (ctx, index) =>
        ctx.path -> index
    }.toMap
    Ast9.GuestModule(ctxs.map(ctx => buildGuestFhunk(module, index)(ctx)))
  }

  def extractCtx(ctx: Ctx5[Expr]): Vector[Ctx5[Expr]] = {
    ctx match {
      case ctx: Ctx5.ExtendCtx5 => extractCtx(ctx.bindings) ++ extractCtx(ctx.body)
      case ctx: Ctx5.ListCtx5 =>
        ctx.items
          .map { item =>
            extractCtx(item)
          }
          .foldLeft(Vector.empty[Ctx5[Expr]])(_ ++ _)
      case ctx: Ctx5.DictCtx5 =>
        ctx.items.values
          .map { item =>
            extractCtx(item)
          }
          .foldLeft(Vector.empty[Ctx5[Expr]])(_ ++ _)
      case ctx: Ctx5.FunCtx5 =>
        extractCtx(ctx.body).prepended(ctx)
      case ctx: Ctx5.ApplyCtx5 =>
        extractCtx(ctx.fn) ++ extractCtx(ctx.arg0) ++ ctx.args
          .map { item =>
            extractCtx(item)
          }
          .foldLeft(Vector.empty[Ctx5[Expr]])(_ ++ _)
      case ctx: Ctx5.RecLetCtx5 =>
        ctx.bindings.valuesIterator
          .map { item =>
            extractCtx(item)
          }
          .foldLeft(Vector.empty[Ctx5[Expr]])(_ ++ _) ++ extractCtx(ctx.body)
      case _ =>
        Vector.empty
    }
  }
  def extract(expr: Ast5.Expr): Vector[Ctx5[Expr]] = {
    val rootCtx = Ctx5.root(expr)
    extractCtx(rootCtx).prepended(rootCtx)
  }

  override def parse(module: String, ast: Ast5): Task[Ast9.GuestModule] = {
    ast match {
      case expr: Ast5.Expr => ZIO.attempt(parse(module, extract(expr)))
    }
  }
}
