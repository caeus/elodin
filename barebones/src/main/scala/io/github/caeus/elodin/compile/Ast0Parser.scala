package io.github.caeus.elodin.compile

import io.github.caeus.elodin.compile.Ast0.BlockStatement.{DoStatement, FunStatement, LetStatement}
import io.github.caeus.elodin.compile.Ast0.Expr._
import io.github.caeus.elodin.compile.Ast0.ForStatement.{DoBangStatement, LetBangStatement}
import io.github.caeus.elodin.compile.Ast0.{Expr, ForStatement}
import io.github.caeus.elodin.compile.Token.MetaToken
import io.github.caeus.elodin.compile.util.Splitting.{Branch, Leaf}
import io.github.caeus.elodin.compile.util.{SepEl, SepNel, SplitTree}
import io.github.caeus.elodin.util.Pos2Loc
import io.github.caeus.elodin.value.Eson
import io.github.caeus.plutus.PackerSyntax.VectorPackerSyntax
import io.github.caeus.plutus.{Packer, PrettyPacker}
import zio._

private[elodin] sealed trait NextForm
object NextForm {
  final case class Select(expr: Expr)                extends NextForm
  final case class Args(arg0: Expr, args: Seq[Expr]) extends NextForm
  final case class BlockOrMonad(expr: Expr)          extends NextForm
}

trait Ast0Parser {
  def parse(tokens: Seq[MetaToken[Pos2Loc.Loc]]): Task[Ast0]
}
object Ast0Parser {
  def make: Ast0Parser = new LiveAst0Parser
}

final class LiveAst0Parser extends Ast0Parser {

  val syntax = new VectorPackerSyntax[MetaToken[Pos2Loc.Loc]]()
  import syntax._

  type Parser[Out] = Packer[Vector[MetaToken[Pos2Loc.Loc]], MetaToken[Pos2Loc.Loc], Out]

  def P(token: Token) =
    fromPartial {
      case MetaToken(tkn, _) if tkn == token => ()
    }

  def loc[Out](parser: Parser[Pos2Loc.Loc => Out]): Parser[Out] = {
    Packer.make { cursor =>
      parser.take(cursor).map(f => f(cursor.elem(cursor.pos).meta))
    }
  }

  lazy val opPart =
    P(Token.Parenthesis.Open) ~ opSep.map(op => "(" ++ op.to ++ ")") ~ P(Token.Parenthesis.Close)

  lazy val safePart = fromPartial {
    case MetaToken(Token.Identifier(n), _) => n
  }
  lazy val refPart: Parser[String] = opPart | safePart

  lazy val refString: Parser[String] = (refPart ~ (P(Token.RefSep) ~ refPart).rep()).map {
    case (head, tail) => tail.prepended(head).mkString("#")
  }
  lazy val refExpr: Parser[RefExpr] = loc(refString.map(s => loc => RefExpr(s, loc)))

  lazy val valueExpr: Parser[EsonExpr] = loc(fromPartial {
    case MetaToken(Token.Literal(value), _) => loc => EsonExpr(value, loc)
  })

  lazy val letStatement: Parser[Ast0.BlockStatement.LetStatement] = loc {
    (P(Token.Let) ~ refExpr ~ P(Token.Equals) ~ expression).map {
      case (bind, to) => loc => LetStatement(bind.to, to, loc)
    }
  }

  lazy val listExpr =
    loc(
      P(Token.Bracket.Open) ~ expression
        .rep(0, None, P(Token.Comma))
        .map { items => loc => ListExpr(items, loc) } ~ P(Token.Bracket.Close)
    )

  lazy val dictExpr = loc(
    P(Token.Curly.Open) ~ (refExpr ~ P(Token.Equals) ~ expression)
      .rep(0, None, P(Token.Comma))
      .map { fields: Seq[(RefExpr, Expr)] => loc =>
        DictExpr(
          fields.map {
            case (ref, expr) => ref.to -> expr
          }.toMap,
          loc
        )
      } ~ P(
      Token.Curly.Close
    )
  )
  lazy val groupedExpr: Parser[Expr] =
    P(Token.Parenthesis.Open) ~ expression ~ P(Token.Parenthesis.Close)

  lazy val blockStatement: Parser[Ast0.BlockStatement] = letStatement | doStatement | funStatement
  lazy val blockInnards: Parser[BlockExpr] =
    loc(((blockStatement ~ P(Token.Semicolon)).rep ~ expression).map {
      case (statements, last) => loc => BlockExpr(statements, last, loc)
    })

  lazy val bang: Parser[Unit] = P(Token.Operator("!"))

  lazy val doBangStatement: Parser[DoBangStatement] = loc(
    P(Token.Do) ~ bang ~ expression.map(s => loc => DoBangStatement(s, loc))
  )
  lazy val letBangStatement: Parser[LetBangStatement] = loc {
    (P(Token.Let) ~ bang ~ refExpr ~ P(Token.Equals) ~ expression).map {
      case (bind, to) => loc => LetBangStatement(bind.to, to, loc)
    }
  }
  lazy val forStatement: Parser[ForStatement] =
    blockStatement.map(_.toFor) | letBangStatement | doBangStatement

  lazy val funHeader: Parser[(String, Vector[String])] =
    (refString ~ (P(Token.Comma) ~ refString).rep()) ~ P(
      Token.Arrow
    )

  lazy val monadHeader: Parser[Expr] = expression ~ P(Token.For)
  lazy val monadInnards: Parser[ApplyExpr] = {
    loc(
      (monadHeader ~ (forStatement ~ P(Token.Semicolon)).rep() ~ expression)
        .map {
          case (interpreter, statements: Seq[ForStatement], last) =>
            loc =>
              ApplyExpr(
                fn = interpreter,
                arg0 =
                  ForExpr(statements, last, statements.headOption.map(_.loc).getOrElse(last.loc)),
                args = Nil,
                loc = loc
              )
        }
    )
  }

  lazy val curlyEnclosedExpr: Parser[Expr] = loc {
    (P(Token.Curly.Open) ~
      funHeader.? ~ (monadInnards | blockInnards) ~ P(Token.Curly.Close)).map {
      case (funHeader, body) =>
        loc =>
          funHeader
            .map {
              case (param0, params) =>
                FunExpr(param0, params, body, loc)
            }
            .getOrElse(body)
    }
  }

  lazy val delimitedExpr: Parser[Expr] =
    curlyEnclosedExpr | groupedExpr | valueExpr | refExpr | listExpr | dictExpr

  lazy val doStatement: Parser[Ast0.BlockStatement.DoStatement] = loc {
    P(Token.Do) ~ expression.map(s => loc => DoStatement(s, loc))
  }

  lazy val funStatement: Parser[FunStatement] = loc {
    (P(Token.Fun) ~ refString ~ P(Token.Parenthesis.Open) ~
      refString ~ (P(Token.Comma) ~ refString).rep() ~ P(
      Token.Parenthesis.Close
    ) ~ P(Token.Equals) ~ expression).map {
      case (bind, param0, params, body) => loc => FunStatement(bind, param0, params, body, loc)
    }
  }

  lazy val selectForm = P(Token.Dot) ~ delimitedExpr.map(NextForm.Select.apply)
  lazy val argsForm =
    (P(Token.Parenthesis.Open) ~ expression ~ (P(Token.Comma) ~ expression).rep ~ P(
      Token.Parenthesis.Close
    )).map {
      case (arg0, args) => NextForm.Args(arg0, args)
    }

  lazy val nextForm = selectForm | argsForm | curlyEnclosedExpr.map(NextForm.BlockOrMonad.apply)

  lazy val composeExprOnly: Parser[Expr] = loc {
    (delimitedExpr ~ nextForm.rep(1)).map {
      case (head: Expr, tail: Seq[NextForm]) =>
        loc =>
          tail.foldLeft(head) { (acum, curr) =>
            curr match {
              case NextForm.Select(expr) =>
                ApplyExpr(expr, acum, Nil, loc)
              case NextForm.Args(arg0, args) =>
                ApplyExpr(acum, arg0, args, loc)
              case NextForm.BlockOrMonad(expr) =>
                ApplyExpr(acum, expr, Nil, loc)
            }
          }
    }
  }

  lazy val withComposeExpr: Parser[Expr] = delimitedExpr | composeExprOnly

  lazy val opSep: Parser[RefExpr] = loc(fromPartial {
    case MetaToken(Token.Operator(name), _) => loc => RefExpr("(" ++ name ++ ")", loc)
  })

  lazy val unaryExprOnly = loc {
    (loc(P(Token.Operator("-")).as(loc => RefExpr("(-)", loc)))
      ~ withComposeExpr).map {
      case (minus, expr) => loc => ApplyExpr(minus, EsonExpr(Eson.IntVal(0), loc), Seq(expr), loc)
    }
  }

  lazy val withUnaryExpr = withComposeExpr | unaryExprOnly

  lazy val inlineExprOnly: Parser[Expr] =
    (withUnaryExpr ~ (opSep ~ withComposeExpr).rep(1)).map {
      case (head, tail) =>
        splitTreeToApply(
          SepNel(
            head,
            tail.map {
              case (op, expr) => SepEl(op, expr)
            }.toList
          ).splitFull(Ordering.by[RefExpr, String](_.to))
        )
    }
  lazy val withInlineExpr: Parser[Expr] = withUnaryExpr | inlineExprOnly

  lazy val expression: Parser[Expr] = withInlineExpr
  lazy val prettyPacker             = PrettyPacker.version1(blockInnards ~ End)
  override def parse(tokens: Seq[MetaToken[Pos2Loc.Loc]]): IO[Throwable, Ast0] = {
    ZIO.fromEither(
      prettyPacker.process(tokens.toVector)
    )
  }

  private def splitTreeToApply(splitTree: SplitTree[Expr, RefExpr]): Expr = {
    def isLeftAssociative(sep: RefExpr): Boolean = !sep.to.endsWith(":)")
    splitTree match {
      case Branch(sep, parts) =>
        val nodes = parts.map(splitTreeToApply)
        if (isLeftAssociative(sep)) {
          nodes.reduceLeft { (nodeL, nodeR) =>
            ApplyExpr(sep, nodeL, List(nodeR), sep.loc)
          }
        } else {
          nodes.reduceRight { (nodeL, nodeR) =>
            ApplyExpr(sep, nodeL, List(nodeR), sep.loc)
          }
        }
      case Leaf(el) => el
    }
  }
}
