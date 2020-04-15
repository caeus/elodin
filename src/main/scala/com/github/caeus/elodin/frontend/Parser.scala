package com.github.caeus.elodin.frontend

import com.github.caeus.elodin.frontend.ElodinToken._
import com.github.caeus.elodin.lang.Node
import com.github.caeus.elodin.lang.Node.{
  ApplyNode,
  ArrNode,
  DictNode,
  FnNode,
  IntNode,
  LetNode,
  RefNode,
  TextNode
}
import com.github.caeus.plutus.PackerResult.{Done, Failed}
import com.github.caeus.plutus.SyntaxSugar.VectorSyntaxSugar
import com.github.caeus.plutus.syntax._
import com.github.caeus.plutus.{Cursor, Packer}
import zio.Task

class Parser {
  type Pckr[Out] = Packer[Vector[ElodinToken], ElodinToken, Out]

  private val syntax = new VectorSyntaxSugar[ElodinToken]
  import syntax._

  lazy val numLiteral: Pckr[IntNode] = Packer.failed("Numbers not supported yet")

  lazy val expr: Pckr[Node] = (textExpr.named("textOutside") |
    letExpr.named("letOutside") |
    refExpr.named("refOutside") |
    fnExpr.named("fnOutside") |
    arrExpr.named("arrOutside") |
    dictExpr.named("dictOutside") |
    applyExpr.named("applyOutside") |
    numLiteral).named("expr")

  lazy val textExpr: Pckr[TextNode] = Packer.failed("Text not supported yet")

  lazy val refExpr: Pckr[RefNode] = P[RefNode] {
    case Reference(to) => RefNode(to)
  }

  lazy val applyExpr: Pckr[ApplyNode] =
    P(Parenthesis.Open) ~ expr.logging("ARG").rep.map(ApplyNode.apply) ~ P(Parenthesis.Close)

  lazy val dictExpr: Pckr[DictNode] =
    P(Curly.Open) ~ (refExpr ~ P(Colon) ~ expr).rep.map { items =>
      DictNode(items.map {
        case (RefNode(to), node) =>
          to -> node
      }.toMap)
    } ~ P(Curly.Close)

  lazy val arrExpr: Pckr[ArrNode] = P(Bracket.Open) ~ expr.rep.map(ArrNode.apply) ~ P(Bracket.Close)

  lazy val letExpr: Packer[Vector[ElodinToken], ElodinToken, LetNode] =
    (P(Parenthesis.Open) ~ P(Let) ~ dictExpr.named("Letbindings") ~ expr ~ P(Parenthesis.Close))
      .map {
        case (DictNode(items), node) => LetNode(items, node)
      }

  lazy val fnExpr: Packer[Vector[ElodinToken], ElodinToken, FnNode] = {
    val params = fromPartial {
      case Reference(to) => to
    }.rep(min = 1)

    (P(Parenthesis.Open, Fn, Bracket.Open) ~
      params ~ P(Bracket.Close) ~ expr ~ P(Parenthesis.Close))
      .map {
        case (params, node) => FnNode(params, node)
      }
  }

  def parse(seq: Seq[ElodinToken]): Task[Node] = {
    Task.effectSuspend {
      println(seq.length, "----------------------------------------")
      (expr ~ End).take(Cursor.fromSeq(seq)) match {
        case Done(result, _) => Task.succeed(result)
        case f @ Failed(error) =>
          Task.fail(new Exception(s"Failed with message: \n${f.report(seq.toVector)}"))
      }
    }
  }
}
