package com.github.caeus.elodin.compile

import com.github.caeus.elodin.lang.Node._
import com.github.caeus.elodin.lang.{Node, Path, Step}
import zio.Task

import scala.reflect.ClassTag

case class Meta[+A](value: A, children: Map[Step, Meta[A]] = Map.empty[Step, Meta[A]]) {
  def /(step: Step): Meta[A]            = children(step)
  final def key(value: String): Meta[A] = /(Step.Key(value))
  final def index(value: Int): Meta[A]  = /(Step.Index(value))
  final def down: Meta[A]               = /(Step.Down)
}



object NodeOf {
  def unapply(ctxNode: CtxNode): Option[(CtxNode, Node)] = {
    Some(ctxNode -> ctxNode.node)
  }
}

sealed trait CtxNode {

  final def nested(path: Path): Task[CtxNode] = {
    if (path.value.isEmpty) Task.succeed(this)
    else {
      /(path.head).flatMap(_.nested(path.tail))
    }
  }

  final def manyKeys(keys: Set[String]): Task[Map[String, CtxNode]] = {
    Task
      .collectAll(keys.toSeq.map { key =>
        this.key(key).map { ctx =>
          key -> ctx
        }
      })
      .map(_.toMap)
  }

  final def manyIndexes(until: Int): Task[List[CtxNode]] = {
    Task.collectAll((0 until until).map(index))
  }



//  final def resolveRef[T](ref: String)(func: RefResolution => Task[T]): Task[T] = {
//    of(ref).flatMap {
//      case NodeOf(declCtx, _: LetNode) =>
//        declCtx.key(ref).flatMap { refdCtx =>
//          func(IsNode(refdCtx))
//        }
//
//      case NodeOf(declCtx, _: LambdaNode) =>
//        Task.effect(declCtx.lastIndexOf(ref)).flatMap { idx =>
//          func(IsParam(declCtx, idx))
//        }
//    }
//  }

  private final def lastIndexOf(to: String): Int = node match {
    case node: LambdaNode =>
      val i = node.params.lastIndexOf(to)
      if (i < 0) ??? else i
    case _ => ???
  }

  final def key(value: String): Task[CtxNode] = /(Step.Key(value))

  final def index(value: Int): Task[CtxNode] = /(Step.Index(value))

  private final def of(ref: String): Task[CtxNode] =
    if (ownVars contains ref) Task.succeed(this)
    else
      parent
        .map { par =>
          par.of(ref)
        }
        .getOrElse(Task.fail(new IllegalStateException("alsdlaskjdlaksdj")))

  final def down: Task[CtxNode] = /(Step.Down)

  def path: Path
  def node: Node

  final def nodeAs[N <: Node: ClassTag]: Task[N] = {
    node match {
      case n: N => Task.succeed(n)
      case _    => Task.fail(new Exception("woeifhejfbnrgbj"))
    }
  }

  def parent: Option[CtxNode]

  final def ownVars: Set[String] = {
    node match {
      case LambdaNode(params, body) => params.toSet
      case LetNode(bindings, body)  => bindings.keySet
      case _ => Set.empty
    }
  }

  final def /(step: Step): Task[CtxNode] = {
    Task.effect {
      new ChildCtxNode(path / step, node / step, this)
    }
  }

}

final class RootCtxNode(val node: Node) extends CtxNode {
  override def path: Path = Path.root

  override def parent: Option[CtxNode] = None
}
final class ChildCtxNode(val path: Path, val node: Node, _parent: CtxNode) extends CtxNode {

  override def parent: Option[CtxNode] = Some(_parent)

}
