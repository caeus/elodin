package com.github.caeus.elodin.compile

import Node._
import DoStep.{BindPart, YieldPart}
import ElodinToken._
import com.github.caeus.elodin.util.Splitting.{Branch, Leaf}
import com.github.caeus.elodin.util.{SepEl, SepNel, SplitTree}
import com.github.caeus.plutus.PackerSyntax.VectorPackerSyntax
import com.github.caeus.plutus.{Packer, PrettyPacker}
import zio.Task

import scala.annotation.tailrec

sealed trait DoStep
object DoStep {
  case class BindPart(to: String, body: Node)  extends DoStep
  case class YieldPart(to: String, body: Node) extends DoStep
  @tailrec
  private def reduceRec(revParts: List[DoStep], curr: Node): Node = {
    revParts match {
      case Nil => curr
      case BindPart(to, body) :: prevs =>
        val newCurr = curr match {
          case LetNode(bindings, letBody) =>
            //Check to not add an already defined binding
            LetNode(bindings.updated(to, body), letBody)
          case _ => LetNode(Map(to -> body), curr)
        }
        reduceRec(prevs, newCurr)
      case YieldPart(to, body) :: prevs =>
        val newCurr = ApplyNode(Seq(QRefNode("gen", "suspend"), body, FunNode(Seq(to), curr)))
        reduceRec(prevs, newCurr)
    }
  }
  def reduce(parts: Seq[DoStep], last: Node): Node = {
    reduceRec(parts.reverse.toList, ApplyNode(Seq(QRefNode("gen", "done"), last)))
  }

}
trait Parser {
  def parse(tokens: Seq[ElodinToken]): Task[Node]
}
object Parser {
  def make: Parser = new DefaultParser
}
final class DefaultParser extends Parser {
  type Pckr[Out] = Packer[Vector[ElodinToken], ElodinToken, Out]

  private val syntax = new VectorPackerSyntax[ElodinToken]
  import syntax._

  lazy val refSet: Pckr[Set[String]] =
    (P(Curly.Open) ~ refExpr.rep(sep = P(Comma)) ~ P(Curly.Close))
      .map(_.map(_.to).toSet)
  lazy val refMap: Pckr[Map[String, String]] =
    (P(Curly.Open) ~ (refExpr ~ P(Equals) ~ refExpr).rep(sep = P(Comma)) ~ P(Curly.Close))
      .map {
        _.map(r => r._1.to -> r._2.to).toMap
      }
  lazy val selectionPart: Pckr[Selection] = (fromPartial {
    case Operator("^") => ()
  }.? ~ refSet ~ refExpr.? ~ refMap.?).map {
    case (complement, named, prefix_?, renamed) =>
      Selection(complement.nonEmpty, named, prefix_?.map(_.to), renamed.getOrElse(Map.empty))
  }

  lazy val importExpr: Pckr[Node.ImportNode] = (P(ElodinToken.Import) ~
    fromPartial {
      case ElodinToken.Text(to) => to
    } ~ selectionPart ~ P(Semicolon) ~ expr).map {
    case (to, selection, body) =>
      Node.ImportNode(to, selection, body)
  }

  lazy val funExpr: Pckr[Node.FunNode] =
    (P(Parenthesis.Open) ~ refExpr.rep((1), sep = P(Comma)) ~ P(
      Parenthesis.Close
    ) ~ P(Fun) ~ expr).map {
      case (args, body) => Node.FunNode(args.map(_.to), body)
    }

  lazy val opExpr: Pckr[RefNode] = fromPartial {
    case Operator(to) => RefNode(to)
  }

  lazy val refExpr: Pckr[Node.RefNode] = ((fromPartial {
    case Reference(to) => RefNode(to)
  } | P(Parenthesis.Open) ~ opExpr ~ P(Parenthesis.Close)) ~
    (P(Dot) ~ refExpr).rep).map {
    case (to, rest) if rest.isEmpty => to
    case (to, rest) =>
      RefNode(
        rest
          .prepended(to)
          .map(_.to)
          .mkString(".")
      )
  }

  lazy val argsPart: Pckr[List[Node]] = ((P(ElodinToken.Parenthesis.Open) ~ expr ~
    (P(ElodinToken.Comma) ~ expr).rep ~
    P(ElodinToken.Parenthesis.Close)).map {
    case (node, nodes) => node :: nodes.toList
  } | delimitedExpr.map(el => List(el))).rep(1).map(_.flatten.toList)

  /**
    * sum3(3,4,6)
    * sum(3)(4,6)
    */
  lazy val applyExpr: Pckr[Node] = ((delimitedExpr ~ argsPart.?) ~
    (opExpr ~ delimitedExpr ~ argsPart.?).rep).map {
    case (node, args_?, postfixes) =>
      splitTreeToApply(
        SepNel(
          args_?
            .map { r =>
              ApplyNode(node :: r)
            }
            .getOrElse(node),
          postfixes.map {
            case (op, node, args_?) =>
              SepEl(
                op.to,
                args_?
                  .map { r =>
                    ApplyNode(node :: r)
                  }
                  .getOrElse(node)
              )
          }.toList
        ).splitFull
      )
  }

  private def splitTreeToApply(splitTree: SplitTree[Node, String]): Node = {
    def isLeftAssociative(sep: String): Boolean = !sep.endsWith(":")
    splitTree match {
      case Branch(sep, parts) =>
        val nodes = parts.map(splitTreeToApply)
        if (isLeftAssociative(sep)) {
          nodes.reduceLeft { (nodeL, nodeR) =>
            ApplyNode(Seq(RefNode(sep), nodeL, nodeR))
          }
        } else {
          nodes.reduceRight { (nodeL, nodeR) =>
            ApplyNode(Seq(RefNode(sep), nodeL, nodeR))
          }
        }
      case Leaf(el) => el
    }
  }

  /**
    * let
    *   x=5;
    *   y=7;
    *   sum(x,y)
    *
    */
  type Binding = (String, Node)
  lazy val letBinding: Pckr[Binding] = (refExpr ~ P(Equals) ~ expr).map {
    case (ref, body) => ref.to -> body
  }
  lazy val letStructure: Pckr[(List[Binding], Node)] =
    expr.map(Nil -> _) | (letBinding ~ P(Semicolon) ~ letStructure).map {
      case (to, expr, structure) =>
        ((to, expr) :: structure._1) -> structure._2
    }
  lazy val letExpr = P(Let) ~ letStructure.map {
    case (bindings, body) =>
      Node.LetNode(
        bindings.toMap,
        body
      )
  }
  lazy val doStructure: Pckr[(List[DoStep], Node)] =
    expr.map { Nil -> _ } | (doStep ~ P(Semicolon) ~ doStructure).map {
      case (step, body) => (step :: body._1) -> body._2
    }

  lazy val doStep: Pckr[DoStep] = ((refExpr ~ P(Yield)).? ~ expr).map {
    case (ref, body) => YieldPart(ref.map(_.to).getOrElse("_"), body)
  } |
    (refExpr ~ P(Equals) ~ expr).map {
      case (ref, body) => BindPart(ref.to, body)
    }

  lazy val doExpr = (P(Do) ~ doStructure).map {
    case (parts, last) =>
      DoStep.reduce(parts, last)
  }

  lazy val dictExpr: Pckr[DictNode] =
    P(Curly.Open) ~ (refExpr ~ P(Equals) ~ expr).rep(sep = P(Comma)).map { items =>
      DictNode(items.map {
        case (RefNode(to), node) => to -> node
      }.toMap)
    } ~ P(Curly.Close)

  lazy val arrExpr: Pckr[ArrNode] = P(Bracket.Open) ~ expr.rep(sep = P(Comma)).map { items =>
    ArrNode(items)
  } ~ P(Bracket.Close)

  lazy val groupedExpr: Pckr[Node] =
    (P(ElodinToken.Parenthesis.Open) ~ expr ~ P(ElodinToken.Parenthesis.Close)) |
      P(ElodinToken.Curly.Open) ~ expr ~ P(ElodinToken.Curly.Close)

  lazy val intLiteral: Pckr[IntNode] = P {
    case IntNum(value) => IntNode(value)
  }
  lazy val floatLiteral: Pckr[FloatNode] = P {
    case FloatNum(value) => FloatNode(value)
  }
  lazy val boolLiteral: Pckr[BoolNode] = P {
    case Bool(value) => BoolNode(value)
  }
  lazy val textLiteral: Pckr[Node.TextNode] = P {
    case Text(value) => TextNode(value)
  }
  lazy val qRefExpr: Pckr[Node.QRefNode] = (textLiteral ~ P(Dot) ~ refExpr).map {
    case (text, ref) => QRefNode(text.value, ref.to)
  }

  lazy val greedyExpr = importExpr.named("GImport") |
    letExpr.named("GLet") |
    funExpr.named("GFun") |
    doExpr.named("GDo") |
    applyExpr.named("GApp")
  lazy val delimitedExpr: Pckr[Node] = refExpr.named("DRef") |
    arrExpr.named("DArr") |
    dictExpr.named("DDict") |
    groupedExpr.named("DGroup") |
    qRefExpr.named("DQRef") |
    intLiteral.named("DInt") |
    floatLiteral.named("DFloat") |
    boolLiteral.named("DBool") |
    textLiteral.named("DText")

  lazy val expr: Pckr[Node] = delimitedExpr.named("Delimited") | greedyExpr.named("Greedy")
  lazy val finalExpr        = expr ~ End
  lazy val prettyPacker     = PrettyPacker.version1(finalExpr)
  def parse(seq: Seq[ElodinToken]): Task[Node] = {
    Task.effectSuspend {
      Task.fromEither(prettyPacker.process(seq.toVector))
    }
  }
}
