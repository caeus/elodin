package io.github.caeus.felurian.compile

import com.jsoniter.Jsoniter
import io.github.caeus.felurian.compile.Token._
import io.github.caeus.felurian.value.Value
import io.github.caeus.plutus.PackerSyntax.StringPackerSyntax
import io.github.caeus.plutus.{Packer, PrettyPacker}
import zio.{Task, ZIO}

trait Lexer {
  def lex(code: String): Task[Seq[Token]]
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

  lazy val _comment: Packer[String, Char, Unit] = P("*/") | (P(_ => true) ~ _comment).as(())
  lazy val comment: Packer[String, Char, Unit]  = (P("/*") ~ _comment)

  lazy val booleanToken: Packer[String, Char, Val] =
    P("""true|false""".r).map(_.toBoolean).map(b => Val(Value.BoolVal(b)))

  lazy val integralToken: Packer[String, Char, Val] =
    integral.!.map(_.value).map { num =>
      Val(Value.IntVal(BigInt(num)))
    }

  lazy val floatingToken: Packer[String, Char, Val] =
    (integral ~ fractional.?.as(()) ~ exponent.?.as(())).!.map(_.value)
      .map(num => Val(Value.FloatVal(BigDecimal(num))))

  lazy val opToken = P(c => opChars.contains(c)).rep.!.map(_.value).map(Operator.apply)

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

  lazy val textToken: Packer[String, Char, Val] =
    (P("\"") ~ (strChars | escape).rep ~ P("\"")).!.map { window =>
      Val(Value.TextVal(Jsoniter.parse(window.value).readString()))
    }

  private val lexerPacker: Packer[String, Char, Vector[Token]] =
    (P("(").as(Some(Parenthesis.Open)) |
      P(")").as(Some(Parenthesis.Close)) |
      P("=>").as(Some(Arrow)) |
      P("""\s+""".r).as(None) |
      P("[").as(Some(Bracket.Open)) |
      P("]").as(Some(Bracket.Close)) |
      P("{").as(Some(Curly.Open)) |
      P("}").as(Some(Curly.Close)) |
      P("#").as(Some(RefSep)) |
      P("fun").as(Some(Fun)) |
      P("do").as(Some(Do)) |
      P(".").as(Some(Dot)) |
      P("let").as(Some(Let)) |
      P("for").as(Some(For)) |
      P("import").as(Some(Import)) |
      P("=").as(Some(Equals)) |
      P(",").as(Some(Comma)) |
      P(";").as(Some(Semicolon)) |
      P("unit").as(Some(Val(Value.UnitVal))) |
      comment.as(None) | comment.as(None) |
      opToken.map(Some.apply) |
      booleanToken.map(Some.apply) |
      idToken.map(Some.apply) |
      integralToken.map(Some.apply) |
      floatingToken.map(Some.apply) |
      textToken.map(Some.apply)).rep
      .map(_.collect[Token] {
        case Some(x) => x
      }) ~ End
  lazy val prettyPacker = PrettyPacker.version1(lexerPacker ~ End)

  override def lex(source: String): Task[Vector[Token]] =
    ZIO.fromEither(prettyPacker.process(source))

}
