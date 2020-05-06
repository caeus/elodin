package com.github.caeus.elodin.interpreter.scope

import com.github.caeus.elodin.interpreter.Step.FnBody
import com.github.caeus.elodin.interpreter.Val.Lazy
import com.github.caeus.elodin.interpreter.{Step, Val}
import com.github.caeus.elodin.lang.Node
import com.github.caeus.elodin.lang.Node._

sealed trait Scope[N <: Node] {
  def node: N
}

object Scope {
  sealed class When[N <: Node: Manifest] {
    def unapply(scope: Scope[Node]): Option[Scope[N]] = scope.narrow[N]
  }
  object WhenLet   extends When[LetNode]
  object WhenFn    extends When[FnNode]
  object WhenRef   extends When[RefNode]
  object WhenApply extends When[ApplyNode]
  object WhenArr   extends When[ArrNode]
  object WhenDict  extends When[DictNode]
  object WhenText   extends When[TextNode]
  object WhenFloat extends When[FloatNode]
  object WhenInt   extends When[IntNode]
  object WhenBool  extends When[BoolNode]

  def root[N <: Node](node: N): Scope[N] = Root(node)

  private[scope] case class Child[N <: Node](node: N, step: Step, parent: Scope[Node])
      extends Scope[N] {
    override def toString: String = parent.toString + "." + step.toString
  }
  private[scope] case class Root[N <: Node](node: N) extends Scope[N] {
    override def toString: String = "\\"
  }

  implicit class PathNodeOps(private val value: Scope[Node]) extends AnyVal {
    def narrow[N <: Node](implicit tag: Manifest[N]): Option[Scope[N]] =
      value.node match {
        case _: N => Some(value.asInstanceOf[Scope[N]])
        case _    => None
      }
  }

  implicit class GPathOps[N <: Node](private val value: Scope[N]) extends AnyVal {
    def widen: Scope[Node] = value.asInstanceOf[Scope[Node]]
    def toLazy: Val.Lazy   = Lazy(value.widen)
    def resolveRef(to: String): Option[Val] = {
      value.widen match {
        case Child(_, FnBody(args), parent) if args.contains(to) =>
          Some(args(to))
        case WhenLet(scope) if scope.node.bindings.contains(to) =>
          Some(scope.binding(to).get.toLazy)
        case Child(_, _, parent) => parent.resolveRef(to)
        case _                   => None

      }
    }
  }
  implicit class LetPathOps(private val value: Scope[LetNode]) extends AnyVal {
    def body: Scope[Node] = Child(value.node.body, Step.Down, value.widen)
    def binding(key: String): Option[Scope[Node]] =
      value.node.bindings.get(key).map { node =>
        Child(node, Step.Key(key), value.widen)
      }
  }
  implicit class FnPathOps(private val value: Scope[FnNode]) extends AnyVal {
    def body(args: Seq[Val]): Option[Scope[Node]] =
      if (args.size == value.node.params.size)
        Some(Child(value.node.body, FnBody(Map(value.node.params.zip(args): _*)), value.widen))
      else None
  }
  implicit class ApplyPathOps(private val value: Scope[ApplyNode]) extends AnyVal {
    def arg(index: Int): Option[Scope[Node]] =
      value.node.args.lift(index).map { node =>
        Child(node, Step.Index(index), value.widen)
      }
    def argScopes: Seq[Scope[Node]] = {
      (0 until value.node.args.size).map(i => arg(i).get)
    }
  }
  implicit class ArrPathOps(private val value: Scope[ArrNode]) extends AnyVal {
    def item(index: Int): Option[Scope[Node]] =
      value.node.items.lift(index).map { node =>
        Child(node, Step.Index(index), value.widen)
      }
    def itemScopes: Seq[Scope[Node]] = {
      (0 until value.node.items.size).map(i => item(i).get)
    }
  }
  implicit class DictPathPps(private val value: Scope[DictNode]) extends AnyVal {
    def item(key: String): Option[Scope[Node]] =
      value.node.items.get(key).map { node =>
        Child(node, Step.Key(key), value.widen)
      }
    def itemScopes: Map[String, Scope[Node]] = {
      value.node.items.keySet.toSeq.map(key => key -> item(key).get).toMap
    }
  }

}
