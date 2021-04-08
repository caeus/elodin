package io.github.caeus.elodin.compile

import io.github.caeus.elodin.compile.Ast0.{BlockStatement, ForStatement}
import io.github.caeus.elodin.compile.Ast5.NonFunExpr
import io.github.caeus.elodin.util.Pos2Loc.Loc
import io.github.caeus.elodin.value.Value
import zio.{Task, ZIO}

trait Ast5Parser {

  def parse(ast0: Ast0): Task[Ast5]
}
object Ast5Parser {
  def make: Ast5Parser = new LiveAst5Parser
}
final class LiveAst5Parser extends Ast5Parser {

  def ast5FunExpr(
      param: String,
      params: Seq[String],
      body: Ast5.Expr,
      loc: Loc
  ): Ast5.Expr.FunExpr = {
    body match {
      case body: NonFunExpr => Ast5.Expr.FunExpr(param, params, body, loc)
      case Ast5.Expr.FunExpr(param_, params_, body, loc) =>
        Ast5.Expr.FunExpr(param, params.appended(param_).appendedAll(params_), body, loc)
    }
  }
  def ast5ApplyExpr(
      fn: Ast5.Expr,
      arg: Ast5.Expr,
      args: Seq[Ast5.Expr],
      loc: Loc
  ): Ast5.Expr.ApplyExpr = {
    fn match {
      case fn: Ast5.NonApplyExpr => Ast5.Expr.ApplyExpr(fn, arg, args, loc)
      case Ast5.Expr.ApplyExpr(fn_, arg_, args_, loc) =>
        Ast5.Expr.ApplyExpr(fn_, arg_, args_.appended(arg).appendedAll(args), loc)
    }
  }

  def genFlatMap(binding: String, expr: Ast5.Expr, cont: Ast5.Expr, loc: Loc): Ast5.Expr = {
    ast5ApplyExpr(
      Ast5.Expr.HostRefExpr("gen#flatMap", loc),
      expr,
      List(ast5FunExpr(binding, Nil, cont, loc)),
      loc
    )
  }
  def genDone(expr: Ast5.Expr, loc: Loc): Ast5.Expr = {
    ast5ApplyExpr(Ast5.Expr.HostRefExpr("gen#done", loc: Loc), expr, Nil, loc)
  }
  def parseForExpr(statements: Seq[Ast0.ForStatement], last: Ast0.Expr): Task[Ast5.Expr] = {
    statements.foldRight(parseExpr(last).map(ex => genDone(ex, last.loc))) { (statement, cont) =>
      cont.flatMap { cont: Ast5.Expr =>
        statement match {
          case ForStatement.LetBangStatement(binding, expr, loc) =>
            parseExpr(expr).map { expr =>
              genFlatMap(binding, expr, cont, loc)
            }
          case ForStatement.DoBangStatement(expr, loc) =>
            parseExpr(expr).map { expr =>
              genFlatMap("", expr, cont, loc)
            }
          case ForStatement.BlockStatement(statement) =>
            chainCont(statement, cont)
        }
      }
    }
  }

  def chainCont(statement: BlockStatement, cont: Ast5.Expr) = {
    statement match {
      case BlockStatement.ImportStatement(elem, loc) =>
        parseExpr(elem).map { elem =>
          Ast5.Expr.ExtendExpr(elem, cont, loc)
        }
      case Ast0.BlockStatement.LetStatement(binding, expr, loc) =>
        parseExpr(expr).map { expr =>
          Ast5.Expr.ApplyExpr(ast5FunExpr(binding, Nil, cont, loc), expr, Nil, loc)
        }
      case Ast0.BlockStatement.DoStatement(expr, loc) =>
        parseExpr(expr).map { expr =>
          Ast5.Expr.ApplyExpr(ast5FunExpr("", Nil, cont, loc), expr, Nil, loc)
        }
      case Ast0.BlockStatement.FunStatement(binding, param, params, body, loc) =>
        parseExpr(body).map { body =>
          val fun = ast5FunExpr(param, params, body, loc)
          cont match {
            case Ast5.Expr.RecLetExpr(bindings, cont, loc) =>
              Ast5.Expr.RecLetExpr(bindings.updated(binding, fun), cont, loc)
            case cont =>
              Ast5.Expr.RecLetExpr(Map(binding -> fun), cont, loc)
          }
        }
    }
  }

  def parseBlockExpr(statements: Seq[Ast0.BlockStatement], last: Ast0.Expr): Task[Ast5.Expr] = {
    statements.foldRight(parseExpr(last)) { (statement, cont) =>
      cont.flatMap { cont: Ast5.Expr =>
        chainCont(statement, cont)
      }
    }
  }

  def parseExpr(expr: Ast0.Expr): Task[Ast5.Expr] = {

    expr match {
      case Ast0.Expr.ListExpr(items, loc) =>
        ZIO.collectAll(items.map { expr => parseExpr(expr) }).map(v => Ast5.Expr.ListExpr(v, loc))
      case Ast0.Expr.DictExpr(items, loc) =>
        ZIO
          .collectAll(items.map {
            case (k, expr) => parseExpr(expr).map(k -> _)
          })
          .map(v => Ast5.Expr.DictExpr(v.toMap, loc))
      case Ast0.Expr.ReqExpr(to, loc) => ZIO.succeed(Ast5.Expr.ReqExpr(to, loc))
      case Ast0.Expr.BlockExpr(statements, last, loc) =>
        parseBlockExpr(statements, last)
      case Ast0.Expr.FunExpr(param0, params, body, loc) =>
        parseExpr(body).map { body =>
          ast5FunExpr(param0, params, body, loc)
        }
      case Ast0.Expr.ForExpr(statements, last, _) => parseForExpr(statements, last)
      case Ast0.Expr.EsonExpr(value, loc)           => ZIO.succeed(Ast5.Expr.EsonExpr(value, loc))
      case Ast0.Expr.RefExpr(value, loc)            => ZIO.succeed(Ast5.Expr.RefExpr(value, loc))
      case Ast0.Expr.ApplyExpr(fn, arg, args, loc) =>
        ZIO.mapN(parseExpr(fn), parseExpr(arg), ZIO.collectAll(args.map(parseExpr))) {
          (fn, arg, args) => ast5ApplyExpr(fn, arg, args, loc)
        }
    }
  }
  override def parse(ast0: Ast0): Task[Ast5] =
    ast0 match {
      case expr: Ast0.Expr => parseExpr(expr)
    }
}
