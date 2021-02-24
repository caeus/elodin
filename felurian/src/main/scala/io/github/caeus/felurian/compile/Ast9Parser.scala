package io.github.caeus.felurian.compile

import io.github.caeus.felurian.compile.Ast0.{BlockStatement, ForStatement}
import io.github.caeus.felurian.compile.Ast9.NonFunExpr
import zio.{Task, ZIO}

trait Ast9Parser {

  def parse(ast0: Ast0): Task[Ast9]
}
final class LiveAst9Parser extends Ast9Parser {

  def ast5FunExpr(param: String, params: Seq[String], body: Ast9.Expr): Ast9.Expr.FunExpr = {
    body match {
      case body: NonFunExpr => Ast9.Expr.FunExpr(param, params, body)
      case Ast9.Expr.FunExpr(param_, params_, body) =>
        Ast9.Expr.FunExpr(param, params.appended(param_).appendedAll(params_), body)
    }
  }
  def ast5ApplyExpr(fn: Ast9.Expr, arg: Ast9.Expr, args: Seq[Ast9.Expr]): Ast9.Expr.ApplyExpr = {
    fn match {
      case fn: Ast9.NonApplyExpr => Ast9.Expr.ApplyExpr(fn, arg, args)
      case Ast9.Expr.ApplyExpr(fn_, arg_, args_) =>
        Ast9.Expr.ApplyExpr(fn_, arg_, args_.appended(arg).appendedAll(args))
    }
  }

  def genFlatMap(binding: String, expr: Ast9.Expr, cont: Ast9.Expr): Ast9.Expr = {
    ast5ApplyExpr(Ast9.Expr.ForeignExpr("gen#flatMap"), expr, List(cont))
  }
  def genDone(expr: Ast9.Expr): Ast9.Expr = {
    ast5ApplyExpr(Ast9.Expr.ForeignExpr("gen#done"), expr, Nil)
  }
  def parseForExpr(statements: Seq[Ast0.ForStatement], last: Ast0.Expr): Task[Ast9.Expr] = {
    statements.foldRight(parseExpr(last).map(genDone)) { (statement, cont) =>
      cont.flatMap { cont: Ast9.Expr =>
        statement match {
          case ForStatement.LetBangStatement(binding, expr) =>
            parseExpr(expr).map { expr =>
              genFlatMap(binding, expr, cont)
            }
          case ForStatement.DoBangStatement(expr) =>
            parseExpr(expr).map { expr =>
              genFlatMap("", expr, cont)
            }
          case ForStatement.BlockStatement(statement) =>
            chainCont(statement, cont)
        }
      }
    }
  }

  def chainCont(statement: BlockStatement, cont: Ast9.Expr) = {
    statement match {
      case BlockStatement.ImportStatement(elem) =>
        parseExpr(elem).map { elem =>
          Ast9.Expr.SpreadExpr(elem, cont)
        }
      case Ast0.BlockStatement.LetStatement(binding, expr) =>
        parseExpr(expr).map { expr =>
          Ast9.Expr.ApplyExpr(ast5FunExpr(binding, Nil, cont), expr, Nil)
        }
      case Ast0.BlockStatement.DoStatement(expr) =>
        parseExpr(expr).map { expr =>
          Ast9.Expr.ApplyExpr(ast5FunExpr("", Nil, cont), expr, Nil)
        }
      case Ast0.BlockStatement.FunStatement(binding, param, params, body) =>
        parseExpr(body).map { body =>
          val fun = ast5FunExpr(param, params, body)
          cont match {
            case Ast9.Expr.RecLetExpr(bindings, cont) =>
              Ast9.Expr.RecLetExpr(bindings.updated(binding, fun), cont)
            case cont =>
              Ast9.Expr.RecLetExpr(Map(binding -> fun), cont)
          }
        }
    }
  }

  def parseBlockExpr(statements: Seq[Ast0.BlockStatement], last: Ast0.Expr): Task[Ast9.Expr] = {
    statements.foldRight(parseExpr(last)) { (statement, cont) =>
      cont.flatMap { cont: Ast9.Expr =>
        chainCont(statement, cont)
      }
    }
  }

  def parseExpr(expr: Ast0.Expr): Task[Ast9.Expr] = {

    expr match {
      case Ast0.Expr.ListExpr(items) =>
        ZIO.collectAll(items.map { expr => parseExpr(expr) }).map(v => Ast9.Expr.ListExpr(v))
      case Ast0.Expr.DictExpr(items) =>
        ZIO
          .collectAll(items.map {
            case (k, expr) => parseExpr(expr).map(k -> _)
          })
          .map(v => Ast9.Expr.DictExpr(v.toMap))
      case Ast0.Expr.ReqExpr(to) => ZIO.succeed(Ast9.Expr.ReqExpr(to))
      case Ast0.Expr.BlockExpr(statements, last) =>
        parseBlockExpr(statements, last)
      case Ast0.Expr.FunExpr(param0, params, body) =>
        parseExpr(body).map { body =>
          ast5FunExpr(param0, params, body)
        }
      case Ast0.Expr.ForExpr(statements, last) => parseForExpr(statements, last)
      case Ast0.Expr.ValueExpr(value)          => ZIO.succeed(Ast9.Expr.ValueExpr(value))
      case Ast0.Expr.RefExpr(value)            => ZIO.succeed(Ast9.Expr.RefExpr(value))
      case Ast0.Expr.ApplyExpr(fn, arg, args) =>
        ZIO.mapN(parseExpr(fn), parseExpr(arg), ZIO.collectAll(args.map(parseExpr))) {
          (fn, arg, args) => ast5ApplyExpr(fn, arg, args)
        }
    }
  }
  override def parse(ast0: Ast0): Task[Ast9] =
    ast0 match {
      case expr: Ast0.Expr => parseExpr(expr)
    }
}
