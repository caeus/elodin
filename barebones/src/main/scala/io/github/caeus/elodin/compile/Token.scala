package io.github.caeus.elodin.compile

import io.github.caeus.elodin.value.Eson

sealed trait Token
object Token {
  final case class MetaToken[+M](token: Token, meta: M) {
    def map[T](f: M => T): MetaToken[T] = MetaToken(token, f(meta))
  }
  sealed trait Delimiter { del =>
    case object Open extends Token {
      override def toString: String = s"$del.Open"
    }
    case object Close extends Token {
      override def toString: String = s"$del.Closed"
    }
  }
  final case object Curly                   extends Delimiter
  final case object Bracket                 extends Delimiter
  final case object Parenthesis             extends Delimiter
  final case class Literal(value: Eson)     extends Token
  final case object Do                      extends Token
  final case object Let                     extends Token
  final case object For                     extends Token
  final case object Fun                     extends Token
  final case object Module                  extends Token
  final case object Import                  extends Token
  final case object Comma                   extends Token
  final case object Semicolon               extends Token
  final case object Colon                   extends Token
  final case object Arrow                   extends Token
  final case object Equals                  extends Token
  final case class Identifier(name: String) extends Token
  final case object RefSep                  extends Token
  final case class Operator(name: String)   extends Token
  final case object Dot                     extends Token

}
