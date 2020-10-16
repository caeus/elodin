package io.github.caeus.chusky.internal

import io.github.caeus.chusky.internal.CCursor.AliasCursor

final case class LocalizedError(path: List[String], error: String)

object KindInferer {

  private def allAreStar(cursors: Seq[CCursor], newTrace: Set[List[String]]) = {
    val errors = cursors.flatMap { c =>
      (for {
        kind <- infer(c, newTrace)
        _ <- if (kind == KindExpr.Star) {
              Right(KindExpr.Star)
            } else {
              Left(
                c.fail(s"kind of ${c.path} must Star but it is $kind instead")
              )
            }
      } yield ()).left.getOrElse(Nil)
    }.toList
    if (errors.isEmpty) {
      Right(KindExpr.Star)
    } else Left(errors)
  }
  def infer(cursor: CCursor, trace: Set[List[String]]): Either[List[LocalizedError], KindExpr] = {
    if (!trace.contains(cursor.path)) {
      val newTrace: Set[List[String]] = trace + cursor.path
      cursor match {
        case cursor: CCursor.ForeignCursor => Right(cursor.kind)
        case cursor: CCursor.TApplyCursor =>
          for {
            funKind <- infer(cursor.function, newTrace)
            argKind <- infer(cursor.arg, newTrace)
            kind <- funKind match {
                     case KindExpr.Star =>
                       Left(
                         cursor.fail(s"Trying to apply $argKind to $funKind")
                       )
                     case KindExpr.Fun(from, to) =>
                       if (from == argKind)
                         Right(to)
                       else
                         Left(
                           cursor.fail(s"Trying to apply $argKind to $funKind")
                         )
                   }
          } yield kind
        case cursor: CCursor.TLambdaCursor =>
          for {
            kind <- infer(cursor.body, newTrace)
          } yield KindExpr.Fun(cursor.param.kind, kind)
        case cursor: CCursor.TRefCursor =>
          cursor
            .resolveRef(cursor.to)
            .map {
              case ResolvedRef.IsParam(_, name) =>
                Right(name.kind)
              case ResolvedRef.IsCursor(cursor) =>
                infer(cursor, newTrace)
              case ResolvedRef.IsImported(_, name) =>
                Right(name.kind)
            }
            .getOrElse(
              Left(
                cursor.fail(s"Undefined reference to ${cursor.to}")
              )
            )
        case cursor: CCursor.TProductCursor =>
          allAreStar(cursor.types, newTrace)
        case cursor: CCursor.TRecordCursor =>
          allAreStar(cursor.fields.values.toSeq, newTrace)
        case cursor: CCursor.TTaggedUnionCursor =>
          allAreStar(
            cursor.options.values.collect {
              case Some(c) => c
            }.toSeq,
            newTrace
          )
        case cursor: CCursor.TUnionCursor =>
          allAreStar(cursor.options, newTrace)
        case _: CCursor.TEnumCursor =>
          Right(KindExpr.Star)
        case cursor: AliasCursor =>
          infer(cursor.definition, newTrace)
        case _ =>
          Left(cursor.fail("Cannot infer kind here"))
      }
    } else {
      cursor match {
        case cursor: CCursor.AliasCursor =>
          cursor.kind
            .map { kind =>
              Right(kind)
            }
            .getOrElse(
              Left(cursor.fail(s"Recursive definition (${cursor.name}) needs kind annotated"))
            )
        case _ =>
          Left(cursor.fail("I Don't get how we got here!"))
      }
    }
  }
}

object ChuskyValidator {

  import CCursor._

  private def validate(cursor: CCursor): List[LocalizedError] = {
    cursor match {
      case cursor: ModuleCursor =>
        cursor.aliases.toList.flatMap {
          case (_, alias) => validate(alias)
        }
      case cursor: AliasCursor =>
        KindInferer
          .infer(cursor.definition, Set(cursor.path))
          .fold(
            identity,
            { kind =>
              cursor.kind
                .map { annotatedKind =>
                  if (annotatedKind == kind) {
                    Nil
                  } else {
                    cursor.fail(s"expected ${cursor.kind}, got $kind instead")
                  }
                }
                .getOrElse(Nil)
            }
          )
      case _ => Nil
    }
  }
  def validate(moduleExpr: ModuleExpr): Seq[LocalizedError] = {
    validate(CCursor.fromModule(moduleExpr))
  }
}
