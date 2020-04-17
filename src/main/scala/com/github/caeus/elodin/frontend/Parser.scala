package com.github.caeus.elodin.frontend

import com.github.caeus.elodin.frontend.ElodinToken._
import com.github.caeus.elodin.lang.Node
import com.github.caeus.elodin.lang.Node._
import com.github.caeus.plutus.PackerResult.{Done, Failed}
import com.github.caeus.plutus.SyntaxSugar.VectorSyntaxSugar
import com.github.caeus.plutus.syntax._
import com.github.caeus.plutus.{Cursor, Packer}
import zio.Task

class Parser {
  type Pckr[Out] = Packer[Vector[ElodinToken], ElodinToken, Out]

  private val syntax = new VectorSyntaxSugar[ElodinToken]
  import syntax._

  lazy val doStep: Pckr[(String, Node)] = {
    ((refExpr ~ P(Colon) ~ expr).rep ~
      refExpr ~ P(Yield) ~ expr).map {
      case (bindings, stepBinding, step) =>
        stepBinding.to -> (if (bindings.nonEmpty) {
                             LetNode(bindings.map {
                               case (ref, node) =>
                                 ref.to -> node
                             }.toMap, step)
                           } else step)
    }
  }

  private def recDoNotation(revSteps: List[(String, Node)], result: ApplyNode): ApplyNode = {
    revSteps match {
      case Nil => result
      case (param, step) :: rest =>
        recDoNotation(rest, ApplyNode(Seq(ReqNode("Gen.chain"), step, FnNode(Seq(param), result))))
    }
  }
  lazy val doNotation: Pckr[ApplyNode] = {
    (P(Parenthesis.Open) ~ P(Do) ~ P(Bracket.Open) ~
      doStep.rep(min = 1) ~ P(Bracket.Close) ~ expr ~ P(Parenthesis.Close)).map {
      case (steps, result) =>
        steps.reverse.toList match {
          case (param, step) :: rest =>
            recDoNotation(rest,
                          ApplyNode(Seq(ReqNode("Gen.map"), step, FnNode(Seq(param), result))))
          case Nil => ???
        }
    }
  }

  lazy val intLiteral: Pckr[IntNode] = P {
    case IntNum(value) => IntNode(value)
  }
  lazy val floatLiteral: Pckr[FloatNode] = P {
    case FloatNum(value) => FloatNode(value)
  }
  lazy val boolLiteral: Pckr[BoolNode] = P {
    case Bool(value) => BoolNode(value)
  }
  lazy val textLiteral: Pckr[TextNode] = P {
    case Text(value) => TextNode(value)
  }

  lazy val expr: Pckr[Node] = (doNotation |
    textExpr.named("textOutside") |
    letExpr.named("letOutside") |
    refExpr.named("refOutside") |
    fnExpr.named("fnOutside") |
    arrExpr.named("arrOutside") |
    dictExpr.named("dictOutside") |
    applyExpr.named("applyOutside") |
    intLiteral |
    floatLiteral |
    boolLiteral |
    textLiteral).named("expr")

  lazy val textExpr: Pckr[TextNode] = Packer.failed("Text not supported yet")

  lazy val refExpr: Pckr[RefNode] = P[RefNode] {
    case Reference(to) => RefNode(to)
  }

  lazy val applyExpr: Pckr[ApplyNode] =
    P(Parenthesis.Open) ~ expr.rep.map(ApplyNode.apply) ~ P(Parenthesis.Close)

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
