package com.github.caeus.elodin.interpreter.scope

import com.github.caeus.elodin.interpreter.Val
import com.github.caeus.elodin.lang.Node.{
  ApplyNode,
  ArrNode,
  BoolNode,
  DictNode,
  FloatNode,
  IntNode,
  FnNode,
  LetNode,
  RefNode,
  ReqNode,
  TextNode
}
import com.github.caeus.elodin.lang.{Node, Step}

sealed trait Scope[N <: Node] {
  def node: N
}

object Scope {
  sealed class When[N <: Node: Manifest] {
    def unapply(scopeK: Scope[Node]): Option[Scope[N]] = scopeK.narrow[N]
  }
  object WhenLet    extends When[LetNode]
  object WhenFn extends When[FnNode]
  object WhenRef    extends When[RefNode]
  object WhenApply  extends When[ApplyNode]
  object WhenReq    extends When[ReqNode]
  object WhenArr    extends When[ArrNode]
  object WhenDict   extends When[DictNode]
  object WhenStr    extends When[TextNode]
  object WhenFloat  extends When[FloatNode]
  object WhenInt    extends When[IntNode]
  object WhenBool   extends When[BoolNode]

  def root[N <: Node](node: N): Scope[N] = Root(node)

  private[scope] trait Deep[N <: Node] extends Scope[N]
  private[scope] case class AppliedTo[N <: Node](deep: Deep[N], args: Seq[Val]) extends Scope[N] {
    override def node: N = deep.node

    override def toString: String = s"$deep[${args.mkString(", ")}]"
  }
  private[scope] case class Child[N <: Node](node: N, step: Step, parent: Scope[Node])
      extends Deep[N] {
    override def toString: String = parent.toString + "." + step.toString
  }
  private[scope] case class Root[N <: Node](node: N) extends Deep[N] {
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
    def applyTo(args: Seq[Val]): Scope[N] = value match {
      case AppliedTo(deep, _args) => AppliedTo(deep, _args.appendedAll(args))
      case path: Deep[N]          => AppliedTo(path, args)
    }
    def args: Option[Seq[Val]] = value match {
      case AppliedTo(_, _args) => Some(_args)
      case _: Deep[N]          => None
    }
    def enclosing: Option[Scope[Node]] = value match {
      case AppliedTo(deep, _)  => deep.enclosing
      case Child(_, _, parent) => Some(parent)
      case _                   => None
    }

    def resolveRef(to: String): Option[Either[Scope[Node], Val]] = {
      value.widen match {
        case WhenLet(scope) if scope.node.bindings.contains(to) =>
          Some(Left(scope.binding(to).get))
        case WhenFn(scope) if scope.node.params.contains(to) =>
          Some(Right(scope.args.get(scope.node.params.lastIndexOf(to))))
        case _ => enclosing.flatMap(_.resolveRef(to))
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
  implicit class LambdaPathOps(private val value: Scope[FnNode]) extends AnyVal {
    def body: Option[Scope[Node]] = value.args.map {
      case args if args.size >= value.node.params.size =>
        Child(value.node.body, Step.Down, value.widen)
    }
  }
  implicit class ApplyPathOps(private val value: Scope[ApplyNode]) extends AnyVal {
    def arg(index: Int): Option[Scope[Node]] = value.node.args.lift(index).map { node =>
      Child(node, Step.Index(index), value.widen)
    }
    def argScopes: Seq[Scope[Node]] = {
      (0 until value.node.args.size).map(i => arg(i).get)
    }
  }
  implicit class ArrPathOps(private val value: Scope[ArrNode]) extends AnyVal {
    def item(index: Int): Option[Scope[Node]] = value.node.items.lift(index).map { node =>
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
