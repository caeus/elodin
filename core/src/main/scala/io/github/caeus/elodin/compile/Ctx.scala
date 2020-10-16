package io.github.caeus.elodin.compile

import io.github.caeus.elodin.compile.Ctx.FunCtx
import io.github.caeus.elodin.compile.ResolvedRef.{IsCtx, IsImported, IsParam}
import io.github.caeus.elodin.core.{Archive, ThunkRef}
import zio.{IO, ZIO}

sealed trait ResolvedRef
object ResolvedRef {
  final case class IsParam(in: FunCtx, index: Int) extends ResolvedRef
  final case class IsCtx(refd: Ctx)                extends ResolvedRef
  final case class IsImported(ref: ThunkRef)       extends ResolvedRef
}

sealed trait Ctx {
  def parent: Option[Ctx]
  def path: Vector[String]
  def resolveRefHere(archive: Archive)(to: String): Option[ResolvedRef]
  final def resolveRef(archive: Archive)(to: String): Option[ResolvedRef] =
    resolveRefHere(archive)(to) match {
      case ref: Some[ResolvedRef] => ref
      case None =>
        parent
          .flatMap { ctx =>
            ctx.resolveRef(archive)(to)
          }
    }
  final def fail(msg: String, msgs: String*): CompileError = {
    CompileError(
      (msg :: msgs.toList).map { msg =>
        LocalizedError(path, msg)
      },
      None
    )
  }
  final def failM(msg: String, msgs: String*): IO[CompileError, Nothing] =
    ZIO.fail(fail(msg, msgs: _*))
}

object Ctx {
  final case class Up(step: String, parent: Ctx)
  sealed abstract class BaseCtx[N](up: Option[Up], node: N) extends Ctx {
    override def parent: Option[Ctx] = up.map(_.parent)

    override def path: Vector[String] =
      up.map {
          case Up(step, parent) => parent.path.appended(step)
        }
        .getOrElse(Nil)

    override def resolveRefHere(archive: Archive)(to: String): Option[ResolvedRef] = None
  }

  private def fromNode(up: Option[Up], node: Node): Ctx = {
    node match {
      case node: Node.LetNode =>
        new LetCtx(up, node)
      case node: Node.FunNode =>
        new FunCtx(up, node)
      case node: Node.AppNode =>
        new AppCtx(up, node)
      case node: Node.TextNode =>
        new TextCtx(up, node)
      case node: Node.IntNode =>
        new IntCtx(up, node)
      case node: Node.FloatNode =>
        new FloatCtx(up, node)
      case node: Node.BoolNode =>
        new BoolCtx(up, node)
      case node: Node.ArrNode =>
        new ArrCtx(up, node)
      case node: Node.DictNode =>
        new DictCtx(up, node)
      case node: Node.RefNode =>
        new RefCtx(up, node)
      case node: Node.ImportNode =>
        new ImportCtx(up, node)
      case node: Node.QRefNode =>
        new QRefCtx(up, node)
    }
  }
  private def fromNode(step: String, parent: Ctx, node: Node): Ctx = {
    fromNode(Some(Up(step, parent)), node)
  }
  def fromNode(node: Node): Ctx = {
    fromNode(None, node)
  }

  final class LetCtx(up: Option[Up], node: Node.LetNode) extends BaseCtx[Node.LetNode](up, node) {
    self =>
    lazy val bindings: Map[String, Ctx] = node.bindings.map {
      case (key, node) => key -> fromNode(s"binding:$key", self, node)
    }
    lazy val body: Ctx = fromNode("body", self, node)

    override def resolveRefHere(archive: Archive)(to: String): Option[ResolvedRef] = {
      bindings.get(to).map(IsCtx)
    }
  }
  final class FunCtx(up: Option[Up], node: Node.FunNode) extends BaseCtx[Node.FunNode](up, node) {
    self =>
    lazy val params: Seq[String] = node.params
    lazy val body: Ctx           = fromNode("body", self, node)

    override def resolveRefHere(archive: Archive)(to: String): Option[ResolvedRef] = {
      val index = node.params.lastIndexOf(node)
      if (index > 0) Some(IsParam(self, index))
      else None
    }
  }
  final class AppCtx(up: Option[Up], node: Node.AppNode) extends BaseCtx[Node.AppNode](up, node) {
    self =>
    lazy val args: Seq[Ctx] = node.args.zipWithIndex.map {
      case (node, index) => fromNode(s"app:$index", self, node)
    }
  }
  final class TextCtx(up: Option[Up], node: Node.TextNode)
      extends BaseCtx[Node.TextNode](up, node) { self => }
  final class IntCtx(up: Option[Up], node: Node.IntNode) extends BaseCtx[Node.IntNode](up, node) {
    self =>
  }
  final class FloatCtx(up: Option[Up], node: Node.FloatNode)
      extends BaseCtx[Node.FloatNode](up, node) { self => }
  final class BoolCtx(up: Option[Up], node: Node.BoolNode)
      extends BaseCtx[Node.BoolNode](up, node) { self => }
  final class ArrCtx(up: Option[Up], node: Node.ArrNode) extends BaseCtx[Node.ArrNode](up, node) {
    self =>
    lazy val items: Seq[Ctx] = node.items.zipWithIndex.map {
      case (node, index) => fromNode(s"item:$index", self, node)
    }
  }
  final class DictCtx(up: Option[Up], node: Node.DictNode)
      extends BaseCtx[Node.DictNode](up, node) { self =>
    lazy val items: Map[String, Ctx] = node.items.map {
      case (key, node) => key -> fromNode(s"item:$key", self, node)
    }
  }
  final class RefCtx(up: Option[Up], node: Node.RefNode) extends BaseCtx[Node.RefNode](up, node) {
    self =>
    lazy val to: String = node.to
  }
  final class ImportCtx(up: Option[Up], node: Node.ImportNode)
      extends BaseCtx[Node.ImportNode](up, node) { self =>
    lazy val selection: Node.Selection = node.selection
    @inline
    private def resolveImport(archive: Archive, actualName: String) = {
      if (node.selection.selected.contains(actualName) ^ node.selection.complement) {
        val ref = ThunkRef(node.module, actualName)
        if (archive.contains(ref))
          Some(IsImported(ref))
        else None
      } else None
    }
    override def resolveRefHere(archive: Archive)(to: String): Option[ResolvedRef] = {
      node.selection.prefix
        .flatMap { prefix =>
          if (to.startsWith(prefix)) {
            val unprefixed = to.substring(prefix.length)
            resolveImport(archive, node.selection.renamed.getOrElse(unprefixed, unprefixed))
          } else None
        }
        .orElse {
          resolveImport(archive, node.selection.renamed.getOrElse(to, to))
        }
    }
  }
  final class QRefCtx(up: Option[Up], node: Node.QRefNode)
      extends BaseCtx[Node.QRefNode](up, node) { self =>
    lazy val member = node.member
    lazy val book   = node.book
  }
}
