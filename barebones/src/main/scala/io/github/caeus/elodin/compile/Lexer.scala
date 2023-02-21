package io.github.caeus.elodin.compile

import com.jsoniter.JsonIterator
import io.github.caeus.elodin.compile.Token.*
import io.github.caeus.elodin.util.Pos2Loc
import io.github.caeus.elodin.value.Eson
import io.github.caeus.plutus.PackerSyntax.StringPackerSyntax
import io.github.caeus.plutus.{Located, Packer, PrettyPacker}
import zio.{Task, ZIO}

trait Lexer {
  def lex(code: String): Task[Seq[MetaToken[Pos2Loc.Loc]]]
}
object Lexer {

  def make: Lexer = new LiveLexer
}
final class LiveLexer extends Lexer {
  type Parser[Out] = Packer[Vector[Token], Token, Out]

  private val syntax = StringPackerSyntax

  import syntax._

  private val opChars = """|^&=¡·\!<>%@?¿:+-*/""".toSet

  lazy val digits: Packer[String, Char, Unit] = P(_.isDigit).rep(min = 1).as(())

  lazy val exponent: Packer[String, Char, Unit] = (P("e|E".r) ~ P("""[+\-]""".r).? ~ digits).as(())

  lazy val fractional: Packer[String, Char, Unit] = P(".") ~ digits

  lazy val integral: Packer[String, Char, Unit] = ((P("0") | P("""[1-9]""".r)) ~ digits.?).as(())

  lazy val comment: Packer[String, Char, Unit] =
    (P("/*") ~ (P(c => c != '*') | (P(c => c == '*') ~ P((c: Char) => c != '/'))).rep ~ P(
      "*/"
    )).ignore

  lazy val booleanToken: Packer[String, Char, Literal] =
    P("""true|false""".r).map(_.toBoolean).map(b => Literal(Eson.BoolVal(b)))

  lazy val integralToken: Packer[String, Char, Literal] =
    integral.!.map(_.value).map { num =>
      Literal(Eson.IntVal(BigInt(num)))
    }

  lazy val floatingToken: Packer[String, Char, Literal] =
    (integral ~ fractional.?.as(()) ~ exponent.?.as(())).!.map(_.value)
      .map(num => Literal(Eson.FloatVal(BigDecimal(num))))

  lazy val opToken: Packer[String, Char, Operator] =
    P(c => opChars.contains(c)).rep.!.map(_.value).map(Operator.apply)

  lazy val idToken: Packer[String, Char, Identifier] =
    (P(c => c.isLetter || c == '_').!.map(_.value) ~ fromPartial {
      case char if char.isLetterOrDigit || char == '_' => char
    }.rep.!.map(_.value))
      .map {
        case (a: String, b: String) =>
          Identifier(a ++ b)
      }

  lazy val hexDigit: Packer[String, Char, String] = P("[0-9a-fA-F]".r)

  lazy val unicodeEscape: Packer[String, Char, Unit] =
    (P("u") ~ hexDigit ~
      hexDigit ~ hexDigit ~ hexDigit).as(())

  lazy val escape: Packer[String, Char, Unit] = P("\\") ~ (P("[\"/\\\\bfnrt]".r) | unicodeEscape)
    .as(())

  def stringChars(c: Char): Boolean = c != '\"' && c != '\\'

  lazy val strChars: Packer[String, Char, String] = P(stringChars _).rep.!.map(_.value)

  lazy val textToken: Packer[String, Char, Literal] =
    (P("\"") ~ (strChars | escape).rep ~ P("\"")).!.map { window =>
      Literal(Eson.TextVal(JsonIterator.parse(window.value).readString()))
    }

  lazy val ignored = comment | P("""\s+""".r).ignore

  private val oneToken: Packer[String, Char, Token] = {
    P("(").as(Parenthesis.Open) |
      P(")").as(Parenthesis.Close) |
      P("=>").as(Arrow) |
      P("[").as(Bracket.Open) |
      P("]").as(Bracket.Close) |
      P("{").as(Curly.Open) |
      P("}").as(Curly.Close) |
      P("#").as(RefSep) |
      P("fun").as(Fun) |
      P("do").as(Do) |
      P(".").as(Dot) |
      P("let").as(Let) |
      P("for").as(For) |
      P("import").as(Import) |
      P("=").as(Equals) |
      P(",").as(Comma) |
      P(";").as(Semicolon) |
      P("unit").as(Literal(Eson.UnitVal)) |
      opToken |
      booleanToken |
      idToken |
      integralToken |
      floatingToken |
      textToken
  }
  lazy val anyToken = ignored.none | oneToken.located.map(Some.apply)

  private val lexerPacker: Packer[String, Char, Vector[MetaToken[Int]]] =
    anyToken.rep
      .map(_.collect[MetaToken[Int]] {
        case Some(Located(el, pos)) => MetaToken(el, pos)
      }) ~ End

  lazy val prettyPacker = PrettyPacker.version1(lexerPacker ~ End)

  override def lex(source: String): Task[Vector[MetaToken[Pos2Loc.Loc]]] = {
    for {
      pos2Loc <- ZIO.attempt(Pos2Loc.fromCode(source))
      r <- ZIO
            .fromEither(prettyPacker.process(source))
            .map(_.map(_.map(pos2Loc.unsafe)))
    } yield r
  }

}
