package io.github.caeus.elodin.compile

import io.github.caeus.elodin.core.{Archive, ThunkRef}
import io.github.caeus.elodin.compile.Dig.Path
import io.github.caeus.elodin.compile.Lexcope2._
import io.github.caeus.elodin.compile.Node._
import io.github.caeus.elodin.compile.RefResolution.{FunParam, LetBinding, ModuleMember}
import zio.{RIO, ZIO}

sealed trait Lexcope2[N <: Node] {
  def node: N
  def path: Dig.Path
  def maybeParent: Option[Lexcope2[Node]]
  private final def continue(ref: String) = {
    maybeParent
      .map(_.resolve(ref))
      .getOrElse(RIO.succeed(None))
  }
  final def resolve(ref: String): ZIO[Archive, CompileError, Option[RefResolution]] = {
    this.widen match {
      case WhenFn(lexcope) if lexcope.node.params.contains(ref) =>
        ZIO.succeed(Some(FunParam(DeclParam(path, lexcope.node.params.lastIndexOf(ref)))))
      case WhenLet(lexcope) if lexcope.node.bindings.contains(ref) =>
        ZIO.succeed(Some(LetBinding(ref, lexcope.binding(ref).get)))
      case WhenImport(lexcope) =>
        lexcope.members.flatMap {
          case mems if mems contains ref =>
            ZIO.succeed(Some(ModuleMember(ThunkRef(lexcope.node.module, ref))))
          case _ => continue(ref)
        }
      case _ =>
        continue(ref)
    }
  }

  override def toString: String = path.mkString("\\", ".", "")
}

object Lexcope2 {
  case class Root[N <: Node](node: N) extends Lexcope2[N] {
    override def path: Path                              = Vector.empty
    override lazy val maybeParent: Option[Lexcope2[Node]] = None
  }
  case class Child[N <: Node](node: N, dig: Dig, parent: Lexcope2[Node]) extends Lexcope2[N] {
    override lazy val path: Path = parent.path.appended(dig)

    override lazy val maybeParent: Option[Lexcope2[Node]] = Some(parent)
  }
  sealed class When[N <: Node: Manifest] {
    def unapply(lexcope: Lexcope2[Node]): Option[Lexcope2[N]] = {
      lexcope.node match {
        case _: N => Some(lexcope.asInstanceOf[Lexcope2[N]])
        case _    => None
      }
    }
  }
  object WhenLet    extends When[LetNode]
  object WhenFn     extends When[FunNode]
  object WhenRef    extends When[RefNode]
  object WhenQRef   extends When[QRefNode]
  object WhenApply  extends When[AppNode]
  object WhenText   extends When[TextNode]
  object WhenFloat  extends When[FloatNode]
  object WhenInt    extends When[IntNode]
  object WhenImport extends When[ImportNode]
  object WhenBool   extends When[BoolNode]
  object WhenDict   extends When[DictNode]
  object WhenArr    extends When[ArrNode]

  implicit final class LexcopeOps[N <: Node](private val value: Lexcope2[N]) extends AnyVal {
    def widen: Lexcope2[Node] = value.asInstanceOf[Lexcope2[Node]]
  }
  implicit final class LetLexcopeOps(private val value: Lexcope2[LetNode]) extends AnyVal {
    def body: Lexcope2[Node] = {
      Child(value.node.body, Dig.Down, value.widen)
    }
    def binding(to: String): Option[Lexcope2[Node]] = {
      value.node.bindings.get(to).map { node =>
        Child(node, Dig.Key(to), value.widen)
      }
    }
  }
  implicit final class ImportLexcopeOps(private val value: Lexcope2[ImportNode]) extends AnyVal {
    def body: Lexcope2[Node] = {
      Child(value.node.body, Dig.Down, value.widen)
    }
    def members: ZIO[Archive, CompileError, Set[String]] =
      for {
        dependencies <- RIO.environment[Archive]
        members <- ZIO
                    .succeed(
                      dependencies
                        .membersOf(value.node.module)
                    )
                    .someOrFail(CompileError("NOT FFOUND!2", None))
        //TODO
      } yield members
  }
  implicit final class FunLexcopeOps(private val value: Lexcope2[FunNode]) extends AnyVal {
    def body: Lexcope2[Node] = {
      Child(value.node.body, Dig.Down, value.widen)
    }
    def params: Set[DeclParam] = {
      value.node.params.zipWithIndex.map {
        case (_, index) => DeclParam(value.path, index)
      }.toSet
    }
  }
  implicit final class ApplyLexcopeOps(private val value: Lexcope2[AppNode]) extends AnyVal {
    def args: Seq[Lexcope2[Node]] = {
      value.node.args.zipWithIndex.map {
        case (node, index) => Child(node, Dig.Index(index), value.widen)
      }
    }
  }
  implicit final class DictLexcopeOps(private val value: Lexcope2[DictNode]) extends AnyVal {
    def item(to: String): Option[Lexcope2[Node]] = {
      value.node.items.get(to).map { node =>
        Child(node, Dig.Key(to), value.widen)
      }
    }
    def items: Map[String, Lexcope2[Node]] = {
      value.node.items.keys.flatMap { key =>
        item(key).map(key -> _).toList
      }.toMap
    }
  }
  implicit final class ArrLexcopeOps(private val value: Lexcope2[ArrNode]) extends AnyVal {
    def item(to: Int): Option[Lexcope2[Node]] = {
      value.node.items.lift(to).map { node =>
        Child(node, Dig.Index(to), value.widen)
      }
    }
    def items: Seq[Lexcope2[Node]] = {
      value.node.items.zipWithIndex.flatMap {
        case (_, key) =>
          item(key).toList
      }
    }
  }

}
