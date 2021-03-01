package io.github.caeus.felurian.compile

import io.github.caeus.felurian.value.Value
import io.github.caeus.felurian.compile.Ast0.BlockStatement.{DoStatement, FunStatement, LetStatement}
import io.github.caeus.felurian.compile.Ast0.Expr._
import io.github.caeus.felurian.compile.Ast0.ForStatement.{DoBangStatement, LetBangStatement}
import io.github.caeus.felurian.compile.Ast0.{Expr, ForStatement}
import io.github.caeus.felurian.compile.util.Splitting.{Branch, Leaf}
import io.github.caeus.felurian.compile.util.{SepEl, SepNel, SplitTree}
import io.github.caeus.plutus.PackerSyntax.VectorPackerSyntax
import io.github.caeus.plutus.{Packer, PrettyPacker}
import zio._

trait Ast0Parser {
  def parse(tokens: Seq[Token]): Task[Ast0]
}
private[felurian] sealed trait NextForm
object NextForm {
  final case class Select(expr: Expr)                extends NextForm
  final case class Args(arg0: Expr, args: Seq[Expr]) extends NextForm
  final case class BlockOrMonad(expr: Expr)          extends NextForm
}
final class LiveAst0Parser extends Ast0Parser {

  val syntax = new VectorPackerSyntax[Token]()
  import syntax._

  type Parser[Out] = Packer[Vector[Token], Token, Out]

  lazy val opPart = P(Token.Parenthesis.Open) ~ opExpr ~ P(Token.Parenthesis.Close)

  lazy val safePart = fromPartial {
    case Token.Identifier(n) => n
  }
  lazy val refPart: Parser[String] = opPart | safePart

  lazy val refString: Parser[String] = (refPart ~ (P(Token.RefSep) ~ refPart).rep()).map {
    case (head, tail) => tail.prepended(head).mkString("#")
  }
  lazy val refExpr: Parser[RefExpr] = refString.map(RefExpr.apply)

  lazy val valueExpr: Parser[ValueExpr] = fromPartial {
    case Token.Val(value) => ValueExpr(value)
  }

  lazy val letStatement: Parser[Ast0.BlockStatement.LetStatement] =
    (P(Token.Let) ~ refExpr ~ P(Token.Equals) ~ expression).map {
      case (bind, to) => LetStatement(bind.to, to)
    }

  lazy val listExpr =
    P(Token.Bracket.Open) ~ expression
      .rep(0, None, P(Token.Comma))
      .map { items => ListExpr(items) } ~ P(Token.Bracket.Close)
  lazy val dictExpr =
    P(Token.Curly.Open) ~ (refExpr ~ P(Token.Equals) ~ expression)
      .rep(0, None, P(Token.Comma))
      .map { fields: Seq[(RefExpr, Expr)] =>
        DictExpr(fields.map {
          case (ref, expr) => ref.to -> expr
        }.toMap)
      } ~ P(
      Token.Curly.Close
    )

  lazy val groupedExpr: Parser[Expr] =
    P(Token.Parenthesis.Open) ~ expression ~ P(Token.Parenthesis.Close)

  lazy val blockStatement: Parser[Ast0.BlockStatement] = letStatement | doStatement | funStatement
  lazy val blockInnards: Packer[Vector[Token], Token, BlockExpr] =
    ((blockStatement ~ P(Token.Semicolon)).rep ~ expression).map {
      case (statements, last) => BlockExpr(statements, last)
    }

  lazy val bang: Parser[Unit] = fromPartial {
    case Token.Operator("!") => ()
  }

  lazy val doBangStatement: Parser[DoBangStatement] =
    P(Token.Do) ~ bang ~ expression.map(s => DoBangStatement(s))
  lazy val letBangStatement: Parser[LetBangStatement] =
    (P(Token.Let) ~ bang ~ refExpr ~ P(Token.Equals) ~ expression).map {
      case (bind, to) => LetBangStatement(bind.to, to)
    }
  lazy val forStatement: Parser[ForStatement] =
    blockStatement.map(_.toFor) | letBangStatement | doBangStatement

  lazy val funHeader: Parser[(String, Vector[String])] =
    (refString ~ (P(Token.Comma) ~ refString).rep()) ~ P(
      Token.Arrow
    )

  lazy val monadHeader: Parser[Expr] = expression ~ P(Token.For)
  lazy val monadInnards: Parser[ApplyExpr] =
    (monadHeader ~ (forStatement ~ P(Token.Semicolon)).rep() ~ expression)
      .map {
        case (interpreter, statements, last) =>
          ApplyExpr(interpreter, ForExpr(statements, last), Nil)
      }

  lazy val curlyEnclosedExpr: Parser[Expr] = {
    P(Token.Curly.Open) ~
      funHeader.? ~ (monadInnards | blockInnards) ~ P(Token.Curly.Close)
  }.map {
    case (funHeader, body) =>
      funHeader
        .map {
          case (param0, params) =>
            FunExpr(param0, params, body)
        }
        .getOrElse(body)
  }

  lazy val delimitedExpr: Parser[Expr] =
    (curlyEnclosedExpr | groupedExpr | valueExpr | refExpr | listExpr | dictExpr)

  lazy val doStatement: Parser[Ast0.BlockStatement.DoStatement] =
    P(Token.Do) ~ expression.map(s => DoStatement(s))

  lazy val funStatement: Parser[FunStatement] =
    (P(Token.Fun) ~ refString ~ P(Token.Parenthesis.Open) ~
      refString ~ (P(Token.Comma) ~ refString).rep() ~ P(
      Token.Parenthesis.Close
    ) ~ P(Token.Equals) ~ expression).map {
      case (bind, param0, params, body) => FunStatement(bind, param0, params, body)
    }

  lazy val selectForm = P(Token.Dot) ~ delimitedExpr.map(NextForm.Select.apply)
  lazy val argsForm =
    (P(Token.Parenthesis.Open) ~ expression ~ (P(Token.Comma) ~ expression).rep ~ P(
      Token.Parenthesis.Close
    )).map {
      case (arg0, args) => NextForm.Args(arg0, args)
    }

  lazy val nextForm = selectForm | argsForm | curlyEnclosedExpr.map(NextForm.BlockOrMonad.apply)

  lazy val composeExprOnly: Parser[Expr] = {
    (delimitedExpr ~ nextForm.rep(1)).map {
      case (head: Expr, tail: Seq[NextForm]) =>
        tail.foldLeft(head) { (acum, curr) =>
          curr match {
            case NextForm.Select(expr) =>
              ApplyExpr(expr, acum, Nil)
            case NextForm.Args(arg0, args) =>
              ApplyExpr(acum, arg0, args)
            case NextForm.BlockOrMonad(expr) =>
              ApplyExpr(acum, expr, Nil)
          }
        }
    }
  }

  lazy val withComposeExpr: Parser[Expr] = delimitedExpr | composeExprOnly

  lazy val opExpr: Parser[String] = fromPartial {
    case Token.Operator(name) => name
  }

  lazy val unaryExprOnly = (fromPartial {
    case Token.Operator("-") => RefExpr("-")
  } ~ withComposeExpr).map {
    case (minus, expr) => ApplyExpr(minus, ValueExpr(Value.IntVal(0)), Seq(expr))
  }

  lazy val withUnaryExpr = withComposeExpr | unaryExprOnly

  lazy val inlineExprOnly: Parser[Expr] =
    (withUnaryExpr ~ (opExpr ~ withComposeExpr).rep(1)).map {
      case (head, tail) =>
        splitTreeToApply(
          SepNel(
            head,
            tail.map {
              case (op, expr) => SepEl(op, expr)
            }.toList
          ).splitFull
        )
    }
  lazy val withInlineExpr: Parser[Expr] = withUnaryExpr | inlineExprOnly

  lazy val expression: Parser[Expr] = withInlineExpr
  lazy val prettyPacker             = PrettyPacker.version1(blockInnards ~ End)
  override def parse(tokens: Seq[Token]): IO[Throwable, Ast0] = {
    ZIO.fromEither(
      prettyPacker.process(tokens.toVector)
    )
  }

  private def splitTreeToApply(splitTree: SplitTree[Expr, String]): Expr = {
    def isLeftAssociative(sep: String): Boolean = !sep.endsWith(":")
    splitTree match {
      case Branch(sep, parts) =>
        val nodes = parts.map(splitTreeToApply)
        if (isLeftAssociative(sep)) {
          nodes.reduceLeft { (nodeL, nodeR) =>
            ApplyExpr(RefExpr(sep), nodeL, List(nodeR))
          }
        } else {
          nodes.reduceRight { (nodeL, nodeR) =>
            ApplyExpr(RefExpr(sep), nodeL, List(nodeR))
          }
        }
      case Leaf(el) => el
    }
  }
}
