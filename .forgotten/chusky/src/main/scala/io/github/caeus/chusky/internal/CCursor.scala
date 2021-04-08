package io.github.caeus.chusky.internal

sealed trait ResolvedRef
object ResolvedRef {
  final case class IsParam(parentCursor: CCursor, name: KindedName) extends ResolvedRef
  final case class IsCursor(cursor: CCursor)                        extends ResolvedRef
  final case class IsImported(module: String, name: KindedName)     extends ResolvedRef
}
sealed trait CCursor {
  def parent: Option[CCursor]
  def path: List[String]
  def resolveRefHere(to: String): Option[ResolvedRef]
  final def resolveRef(to: String): Option[ResolvedRef] =
    resolveRefHere(to).orElse(parent.flatMap(_.resolveRef(to)))
  final def fail(msg: String, msgs: String*): List[LocalizedError] = {
    (msg :: msgs.toList).map { msg =>
      LocalizedError(path, msg)
    }
  }
}

final case class WayUp(step: String, parent: CCursor)
object CCursor {

  def fromModule(expr: ModuleExpr): CCursor = {
    new ModuleCursor(None, expr)
  }
  private def fromStatement(step: String, parent: CCursor, statement: ModuleStatement): CCursor = {
    fromStatement(Some(WayUp(step, parent)), statement)
  }
  private def fromStatement(wayUp: Option[WayUp], statement: ModuleStatement): CCursor = {
    statement match {
      case expr @ ModuleStatement.ForeignExpr(_, _)  => new ForeignCursor(wayUp, expr)
      case expr @ ModuleStatement.AliasExpr(_, _, _) => new AliasCursor(wayUp, expr)
    }
  }
  final def fromStatement(statement: ModuleStatement): CCursor = {
    fromStatement(None, statement)
  }

  final def fromType(typeExpr: TypeExpr): CCursor = fromType(None, typeExpr)
  private def fromType(step: String, parent: CCursor, typeExpr: TypeExpr): CCursor =
    fromType(Some(WayUp(step, parent)), typeExpr)

  private def fromType(wayUp: Option[WayUp], typeExpr: TypeExpr): CCursor = {
    typeExpr match {
      case expr @ TypeExpr.ApplyExpr(_, _)    => new TApplyCursor(wayUp, expr)
      case expr @ TypeExpr.LambdaExpr(_, _)   => new TLambdaCursor(wayUp, expr)
      case expr @ TypeExpr.RefExpr(_)         => new TRefCursor(wayUp, expr)
      case expr @ TypeExpr.ProductExpr(_)     => new TProductCursor(wayUp, expr)
      case expr @ TypeExpr.RecordExpr(_)      => new TRecordCursor(wayUp, expr)
      case expr @ TypeExpr.TaggedUnionExpr(_) => new TTaggedUnionCursor(wayUp, expr)
      case expr @ TypeExpr.UnionExpr(_)       => new TUnionCursor(wayUp, expr)
      case expr @ TypeExpr.EnumExpr(_)        => new TEnumCursor(wayUp, expr)
    }
  }

  sealed abstract class BaseCCursor[Expr](wayUp: Option[WayUp], expr: Expr) extends CCursor {
    override final lazy val parent: Option[CCursor] = wayUp.map(_.parent)

    override final lazy val path: List[String] = wayUp
      .map { wu =>
        wu.parent.path appended wu.step
      }
      .getOrElse(Nil)

    override def resolveRefHere(to: String): Option[ResolvedRef] = None
  }

  final class TEnumCursor(wayUp: Option[WayUp], expr: TypeExpr.EnumExpr)
      extends BaseCCursor[TypeExpr.EnumExpr](
        wayUp,
        expr
      ) {
    cursor =>
    val options: Seq[String] = expr.options
  }
  final class TUnionCursor(wayUp: Option[WayUp], expr: TypeExpr.UnionExpr)
      extends BaseCCursor[TypeExpr.UnionExpr](
        wayUp,
        expr
      ) {
    cursor =>
    val options: Seq[CCursor] = expr.options.zipWithIndex.map {
      case (expr, index) =>
        fromType(s"indexed:$index", cursor, expr)
    }
  }

  final class TTaggedUnionCursor(wayUp: Option[WayUp], expr: TypeExpr.TaggedUnionExpr)
      extends BaseCCursor[TypeExpr.TaggedUnionExpr](
        wayUp,
        expr
      ) { cursor =>
    val options: Map[String, Option[CCursor]] = expr.options.map {
      case (name, maybeExpr) =>
        name -> maybeExpr.map { expr =>
          fromType(s"named:$name", cursor, expr)
        }
    }
  }
  final class TRecordCursor(wayUp: Option[WayUp], expr: TypeExpr.RecordExpr)
      extends BaseCCursor[TypeExpr.RecordExpr](
        wayUp,
        expr
      ) {
    cursor =>
    val fields: Map[String, CCursor] = expr.fields.map {
      case (name, expr) =>
        name -> fromType(s"named:$name", cursor, expr)
    }
  }
  final class TProductCursor(wayUp: Option[WayUp], expr: TypeExpr.ProductExpr)
      extends BaseCCursor[TypeExpr.ProductExpr](
        wayUp,
        expr
      ) { cursor =>
    lazy val types: Seq[CCursor] = expr.types.zipWithIndex.map {
      case (expr: TypeExpr, index) => fromType(s"index:$index", cursor, expr)
    }
  }
  final class TRefCursor(wayUp: Option[WayUp], expr: TypeExpr.RefExpr)
      extends BaseCCursor[TypeExpr.RefExpr](
        wayUp,
        expr
      ) {
    cursor =>
    lazy val to: String = expr.to
  }
  final class TLambdaCursor(wayUp: Option[WayUp], expr: TypeExpr.LambdaExpr)
      extends BaseCCursor[TypeExpr.LambdaExpr](
        wayUp,
        expr
      ) {
    cursor =>
    lazy val param: KindedName = expr.param
    lazy val body: CCursor     = fromType("body", cursor, expr.body)

    override def resolveRefHere(to: String): Option[ResolvedRef] =
      if (param.name == to) Some(ResolvedRef.IsParam(cursor, param))
      else None
  }
  final class TApplyCursor(wayUp: Option[WayUp], expr: TypeExpr.ApplyExpr)
      extends BaseCCursor[TypeExpr.ApplyExpr](
        wayUp,
        expr
      ) {
    cursor =>
    lazy val function: CCursor = fromType("fun", cursor, expr.fun)
    lazy val arg: CCursor      = fromType("arg", cursor, expr.arg)
  }

  final class ForeignCursor(wayUp: Option[WayUp], expr: ModuleStatement.ForeignExpr)
      extends BaseCCursor[ModuleStatement.ForeignExpr](
        wayUp,
        expr
      ) { cursor =>
    lazy val name: String   = expr.name
    lazy val kind: KindExpr = expr.kind
  }
  final class AliasCursor(wayUp: Option[WayUp], expr: ModuleStatement.AliasExpr)
      extends BaseCCursor[ModuleStatement.AliasExpr](
        wayUp,
        expr
      ) { cursor =>
    lazy val name                = expr.name
    lazy val kind                = expr.kind
    lazy val definition: CCursor = fromType("def", cursor, expr.definition)
  }
  final class ModuleCursor(wayUp: Option[WayUp], expr: ModuleExpr)
      extends BaseCCursor[ModuleExpr](
        wayUp,
        expr
      ) {
    cursor =>
    lazy val aliases: Map[String, CCursor] = expr.statements.collect {
      case expr @ ModuleStatement.AliasExpr(_, _, _) =>
        expr.name -> fromStatement(s"alias:${expr.name}", cursor, expr)
    }.toMap
    lazy val foreigns: Map[String, CCursor] = expr.statements.collect {
      case expr @ ModuleStatement.ForeignExpr(_, _) =>
        expr.name -> fromStatement(s"foreign:${expr.name}", cursor, expr)
    }.toMap

    override def resolveRefHere(to: String): Option[ResolvedRef] =
      aliases
        .get(to)
        .map { alias =>
          ResolvedRef.IsCursor(alias)
        }
        .orElse {
          foreigns.get(to).map { foreign =>
            ResolvedRef.IsCursor(foreign)
          }
        }
  }
}
