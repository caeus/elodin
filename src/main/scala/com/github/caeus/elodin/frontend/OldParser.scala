package com.github.caeus.elodin.frontend

import com.github.caeus.elodin.frontend.ElodinToken._
import com.github.caeus.elodin.lang.Node
import com.github.caeus.elodin.lang.Node._
import com.github.caeus.plutus.PackerSyntax.VectorPackerSyntax
import com.github.caeus.plutus.{Packer, PrettyPacker}
import zio.Task

class OldParser {
  type Pckr[Out] = Packer[Vector[ElodinToken], ElodinToken, Out]

  private val syntax = new VectorPackerSyntax[ElodinToken]
  import syntax._

  lazy val doStep: Pckr[(String, Node)] = {
    ((refExpr ~ P(Equals) ~ expr).rep ~
      refExpr ~ P(Yield) ~ expr).map {
      case (bindings, stepBinding, step) =>
        stepBinding.to -> (if (bindings.nonEmpty) {
                             LetNode(
                               bindings.map {
                                 case (ref, node) =>
                                   ref.to -> node
                               }.toMap,
                               step
                             )
                           } else step)
    }
  }

  private def genWrap(node: Node): ApplyNode = {
    //ApplyNode(Seq(ReqNode("Gen.wrap"), node))
    ???
  }
  private def recDoNotation(revSteps: List[(String, Node)], result: ApplyNode): ApplyNode = {
    revSteps match {
      case Nil => result
      case (param, step) :: rest =>
        recDoNotation(
          rest,
          //ApplyNode(Seq(ReqNode("Gen.chain"), genWrap(step), FnNode(Seq(param), result)))
          ???
        )
    }
  }
  lazy val doNotation: Pckr[ApplyNode] = {
    (P(Parenthesis.Open) ~ P(Do) ~ P(Bracket.Open) ~
      doStep.rep(min = 1) ~ P(Bracket.Close) ~ expr ~ P(Parenthesis.Close)).map {
      case (steps, result) =>
        steps.reverse.toList match {
          case (param, step) :: rest =>
            recDoNotation(
              rest,
              //ApplyNode(Seq(ReqNode("Gen.map"), genWrap(step), FnNode(Seq(param), result)))
              ???
            )
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
    reqExpr.named("reqOutside") |
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

  lazy val reqExpr: Pckr[ImportNode] = {
    P(Parenthesis.Open) ~ P(Import) ~ fromPartial {
      case Text(value)=> ???
    } ~
      P(Parenthesis.Close)
    fromPartial {
      case Text(to) => ???
    }
  }

  lazy val objExpr: Pckr[Map[String, Node]] = P(Curly.Open) ~ (refExpr ~ P(Equals) ~ expr).rep.map {
    items =>
      items.map {
        case (RefNode(to), node) => to -> node
      }.toMap
  } ~ P(Curly.Close)

  lazy val dictExpr: Pckr[ApplyNode] = objExpr.map { items =>
    val args = items.toSeq.flatMap {
      case (to, node) =>
        Seq(TextNode(to), node)
    }
    ???
  }

  lazy val arrExpr: Pckr[ApplyNode] = P(Bracket.Open) ~ expr.rep.map { items =>
    ??? : ApplyNode
  } ~ P(Bracket.Close)

  lazy val letExpr: Packer[Vector[ElodinToken], ElodinToken, LetNode] =
    (P(Parenthesis.Open) ~ P(Let) ~ objExpr.named("Letbindings") ~ expr ~ P(Parenthesis.Close))
      .map {
        case (items, node) => LetNode(items, node)
      }

  lazy val fnExpr: Packer[Vector[ElodinToken], ElodinToken, FunNode] = {
    val params = fromPartial {
      case Reference(to) => to
    }.rep(min = 1)

    (P(Parenthesis.Open, Fun, Bracket.Open) ~
      params ~ P(Bracket.Close) ~ expr ~ P(Parenthesis.Close))
      .map {
        case (params, node) => FunNode(params, node)
      }
  }

  lazy val prettyPacker = PrettyPacker.version1(expr ~ End)
  def parse(seq: Seq[ElodinToken]): Task[Node] = {
    Task.effectSuspend {
      Task.fromEither(prettyPacker.process(seq.toVector))
    }
  }
}
