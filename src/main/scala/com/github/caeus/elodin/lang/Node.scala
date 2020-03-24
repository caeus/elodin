package com.github.caeus.elodin.lang

import com.github.caeus.elodin.lang.Node._
import com.github.caeus.elodin.lang.Step.{Down, Index, Key}

import scala.collection.mutable.ListBuffer

case class Meta[+A](value: A, children: List[Meta[A]])

sealed trait Step
object Step {
  case object Down extends Step {
    override def toString: String = "_"
  }
  case class Index(value: Int) extends Step {
    override def toString: String = value.toString
  }
  case class Key(value: String) extends Step {
    override def toString: String = value
  }
}

case class Path(value: Seq[Step]) {
  final def key(value: String): Path = /(Step.Key(value))
  final def index(value: Int): Path  = /(Step.Index(value))
  final def down: Path               = /(Step.Down)
  final def /(step: Step): Path      = Path(value.appended(step))
  final def parent                   = Path(value.init)
  final def head: Step               = value.head
  final def tail: Path               = Path(value.tail)
  override def toString              = "\\" + value.mkString(".")

}
object Path {
  def root = Path(Vector.empty)

}
sealed trait MNode {
  def seal: Node
}
object MNode {

  case class LetMNode(var bindings: scala.collection.mutable.Map[String, MNode], var body: MNode)
      extends MNode {
    override def seal: Node = LetNode(bindings.toSeq.toMap.view.mapValues(_.seal).toMap, body.seal)
  }

  case class LambdaMNode(
      var params: ListBuffer[String],
      var body: MNode
  ) extends MNode {
    override def seal: Node = LambdaNode(params.toSeq, body.seal)
  }

  case class AppMNode(var args: ListBuffer[MNode]) extends MNode {
    override def seal: Node = AppNode(args.toSeq.map(_.seal))
  }
  case class StrMNode(var value: String) extends MNode {
    override def seal: Node = StrNode(value)
  }
  case class IntMNode(var value: BigInt) extends MNode {
    override def seal: Node = IntNode(value)
  }
  case class RealMNode(var value: BigDecimal) extends MNode {
    override def seal: Node = RealNode(value)
  }
  case class BoolMNode(var value: Boolean) extends MNode {
    override def seal: Node = BoolNode(value)
  }
  case class ArrMNode(var items: ListBuffer[MNode]) extends MNode {
    override def seal: Node = ArrNode(items.toSeq.map(_.seal))
  }

  case class RefMNode(var to: String) extends MNode {
    override def seal: Node = RefNode(to)
  }
  case class RequireMNode(var to: String) extends MNode {
    override def seal: Node = ReqNode(to)
  }
}

sealed trait Node {}

object Node {
  implicit class Ops(private val node: Node) extends AnyVal {
    def prettyPrint(nesting: Int): String = pretty(nesting)(node)
    def /(step: Step): Node = (step, node) match {
      case (Down, LambdaNode(_, body))      => body
      case (Down, LetNode(_, body))         => body
      case (Index(index), AppNode(args))    => args(index)
      case (Index(index), ArrNode(items))   => items(index)
      case (Key(key), LetNode(bindings, _)) => bindings(key)
      case (Key(key), DictNode(items))      => items(key)
      case _ =>
        throw new Exception(
          s"Step: $step is not valid for node of type: ${node.getClass.getSimpleName}")
    }
  }

  case class LetNode(bindings: Map[String, Node], body: Node) extends Node

  case class LambdaNode(
      params: Seq[String],
      body: Node
  ) extends Node

  case class AppNode(args: Seq[Node])           extends Node
  case class StrNode(value: String)             extends Node
  case class IntNode(value: BigInt)             extends Node
  case class RealNode(value: BigDecimal)        extends Node
  case class BoolNode(value: Boolean)           extends Node
  case class ArrNode(items: Seq[Node])          extends Node
  case class DictNode(items: Map[String, Node]) extends Node
  case class RefNode(to: String)                extends Node
  case class ReqNode(to: String)          extends Node

  def pretty(nesting: Int)(node: Node): String =
    node match {
      case LetNode(bindings: Map[String, Node], body: Node) =>
        s"""
           |let {
           | ${bindings.view
             .mapValues(pretty(nesting + 1))
             .map {
               case (name, expr) => (" " * nesting) + s"$name = $expr"
             }
             .mkString(",\n")}
           |} in ${pretty(nesting + 1)(body)}
       """.stripMargin
      case LambdaNode(
          params: Seq[String],
          body: Node
          ) =>
        s"""{(${params.mkString(", ")})=>
           |${" " * nesting}${pretty(nesting + 1)(body)}}""".stripMargin
      case AppNode(args) =>
        s"(${args.map(pretty(nesting + 1)).mkString(" ")})"
      case StrNode(value: String) => s""""$value""""
      case IntNode(value)         => value.toString
      case RealNode(value)        => value.toString
      case BoolNode(value)        => value.toString
      case ArrNode(items)         => s"[${items.map(pretty(nesting + 1)).mkString(", ")}]"
      case DictNode(items) =>
        s"""
           |{
           | ${items.view
             .mapValues(pretty(nesting + 1))
             .map {
               case (name, expr) => (" " * nesting) + s"$name = $expr"
             }
             .mkString(",\n")}
           |}
       """.stripMargin
      case RefNode(to)       => to
      case ReqNode(to) => s"require($to)"
    }

}
