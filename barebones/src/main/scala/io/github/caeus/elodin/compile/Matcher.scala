package io.github.caeus.elodin.compile

import scala.util.chaining._

sealed trait Matcher {}
sealed trait Val
object Matcher {
  final case object Pass                                              extends Matcher
  final case class Unapply(f: Val => Option[Val], next: Seq[Matcher]) extends Matcher

  def extract(matcher: Matcher, value: Val): Option[Seq[Val]] = {
    matcher.pipe {
      case Pass => Some(List(value))
      case Unapply(ff, matchers) =>
        ff(value).flatMap { value =>
          extract(matchers.toList, value)
        }
    }
  }
  def extract(matchers: List[Matcher], value: Val): Option[Seq[Val]] = {
    matchers match {
      case head :: next =>
        for {
          values     <- extract(head, value)
          nextValues <- extract(next, value)
        } yield values.appendedAll(nextValues)
      case Nil => Some(Nil)
    }
  }
}
