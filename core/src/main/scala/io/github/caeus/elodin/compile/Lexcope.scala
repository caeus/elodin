package io.github.caeus.elodin.compile

import io.github.caeus.elodin.archive.Dig.Path
import io.github.caeus.elodin.archive.RefResolution.{FunParam, LetBinding, ModuleMember}
import io.github.caeus.elodin.archive.{Archive, DeclParam, Dig, RefResolution}
import io.github.caeus.elodin.compile.Lexcope._
import io.github.caeus.elodin.compile.Node._
import zio.{RIO, ZIO}

sealed trait Lexcope[N <: Node] {
  def node: N
  def path: Dig.Path
  def maybeParent: Option[Lexcope[Node]]
  private final def continue(ref: String) = {
    maybeParent
      .map(_.resolve(ref))
      .getOrElse(RIO.succeed(None))
  }
  final def resolve(ref: String): RIO[Archive, Option[RefResolution]] = {
    this.widen match {
      case WhenFn(lexcope) if lexcope.node.params.contains(ref) =>
        RIO.succeed(Some(FunParam(DeclParam(path, lexcope.node.params.lastIndexOf(ref)))))
      case WhenLet(lexcope) if lexcope.node.bindings.contains(ref) =>
        RIO.succeed(Some(LetBinding(ref, lexcope.binding(ref).get)))
      case WhenImport(lexcope) =>
        lexcope.members.flatMap {
          case mems if mems contains ref =>
            ZIO.succeed(Some(ModuleMember(lexcope.node.module, ref)))
          case _ => continue(ref)
        }
      case _ =>
        continue(ref)
    }
  }

  override def toString: String = path.mkString("\\", ".", "")
}

object Lexcope {
  case class Root[N <: Node](node: N) extends Lexcope[N] {
    override def path: Path                              = Vector.empty
    override lazy val maybeParent: Option[Lexcope[Node]] = None
  }
  case class Child[N <: Node](node: N, dig: Dig, parent: Lexcope[Node]) extends Lexcope[N] {
    override lazy val path: Path = parent.path.appended(dig)

    override lazy val maybeParent: Option[Lexcope[Node]] = Some(parent)
  }
  sealed class When[N <: Node: Manifest] {
    def unapply(lexcope: Lexcope[Node]): Option[Lexcope[N]] = {
      lexcope.node match {
        case _: N => Some(lexcope.asInstanceOf[Lexcope[N]])
        case _    => None
      }
    }
  }
  object WhenLet    extends When[LetNode]
  object WhenFn     extends When[FunNode]
  object WhenRef    extends When[RefNode]
  object WhenQRef   extends When[QRefNode]
  object WhenApply  extends When[ApplyNode]
  object WhenText   extends When[TextNode]
  object WhenFloat  extends When[FloatNode]
  object WhenInt    extends When[IntNode]
  object WhenImport extends When[ImportNode]
  object WhenBool   extends When[BoolNode]
  object WhenDict   extends When[DictNode]
  object WhenArr    extends When[ArrNode]

  implicit final class LexcopeOps[N <: Node](private val value: Lexcope[N]) extends AnyVal {
    def widen: Lexcope[Node] = value.asInstanceOf[Lexcope[Node]]
  }
  implicit final class LetLexcopeOps(private val value: Lexcope[LetNode]) extends AnyVal {
    def body: Lexcope[Node] = {
      Child(value.node.body, Dig.Down, value.widen)
    }
    def binding(to: String): Option[Lexcope[Node]] = {
      value.node.bindings.get(to).map { node =>
        Child(node, Dig.Key(to), value.widen)
      }
    }
  }
  implicit final class ImportLexcopeOps(private val value: Lexcope[ImportNode]) extends AnyVal {
    def body: Lexcope[Node] = {
      Child(value.node.body, Dig.Down, value.widen)
    }
    def members: RIO[Archive, Set[String]] =
      for {
        dependencies <- RIO.environment[Archive]
        members <- dependencies
                    .chaptersOf(value.node.module)
        //TODO
      } yield members
  }
  implicit final class FunLexcopeOps(private val value: Lexcope[FunNode]) extends AnyVal {
    def body: Lexcope[Node] = {
      Child(value.node.body, Dig.Down, value.widen)
    }
    def params: Set[DeclParam] = {
      value.node.params.zipWithIndex.map {
        case (_, index) => DeclParam(value.path, index)
      }.toSet
    }
  }
  implicit final class ApplyLexcopeOps(private val value: Lexcope[ApplyNode]) extends AnyVal {
    def args: Seq[Lexcope[Node]] = {
      value.node.args.zipWithIndex.map {
        case (node, index) => Child(node, Dig.Index(index), value.widen)
      }
    }
  }
  implicit final class DictLexcopeOps(private val value: Lexcope[DictNode]) extends AnyVal {
    def item(to: String): Option[Lexcope[Node]] = {
      value.node.items.get(to).map { node =>
        Child(node, Dig.Key(to), value.widen)
      }
    }
    def items: Map[String, Lexcope[Node]] = {
      value.node.items.keys.flatMap { key =>
        item(key).map(key -> _).toList
      }.toMap
    }
  }
  implicit final class ArrLexcopeOps(private val value: Lexcope[ArrNode]) extends AnyVal {
    def item(to: Int): Option[Lexcope[Node]] = {
      value.node.items.lift(to).map { node =>
        Child(node, Dig.Index(to), value.widen)
      }
    }
    def items: Seq[Lexcope[Node]] = {
      value.node.items.zipWithIndex.flatMap {
        case (_, key) =>
          item(key).toList
      }
    }
  }

}
