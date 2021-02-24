package io.github.caeus.chusky.internal

import io.github.caeus.chusky.internal.ChuskyToken._
import io.github.caeus.chusky.internal.KindExpr.Fun
import io.github.caeus.chusky.internal.ModuleStatement.{AliasExpr, ForeignExpr}
import io.github.caeus.chusky.internal.TypeExpr._
import io.github.caeus.plutus.PackerSyntax.VectorPackerSyntax
import io.github.caeus.plutus.{Packer, PrettyPacker}

sealed trait ModuleStatement {}
object ModuleStatement {
  case class ForeignExpr(name: String, kind: KindExpr) extends ModuleStatement {}
  case class AliasExpr(name: String, kind: Option[KindExpr], definition: TypeExpr)
      extends ModuleStatement {}

}
final case class ImportExpr(module: String) {}

sealed trait KindExpr
object KindExpr {
  final case object Star                             extends KindExpr
  final case class Fun(from: KindExpr, to: KindExpr) extends KindExpr
}
final case class KindedName(name: String, kind: KindExpr)
sealed trait TypeExpr {}
object TypeExpr {
  final case class ApplyExpr(fun: TypeExpr, arg: TypeExpr)                 extends TypeExpr {}
  final case class LambdaExpr(param: KindedName, body: TypeExpr)           extends TypeExpr {}
  final case class RefExpr(to: String)                                     extends TypeExpr {}
  final case class ProductExpr(types: Seq[TypeExpr])                       extends TypeExpr {}
  final case class RecordExpr(fields: Map[String, TypeExpr])               extends TypeExpr {}
  final case class TaggedUnionExpr(options: Map[String, Option[TypeExpr]]) extends TypeExpr {}
  final case class UnionExpr(options: Seq[TypeExpr])                       extends TypeExpr {}
  final case class EnumExpr(options: Seq[String])                          extends TypeExpr {}
}
case class ModuleExpr(imports: Seq[ImportExpr], statements: Seq[ModuleStatement]) {
  val aliases: Map[String, AliasExpr] = statements.collect {
    case expr @ AliasExpr(name, _, _) => name -> expr
  }.toMap
  val foreigns: Map[String, ForeignExpr] = statements.collect {
    case expr @ ForeignExpr(name, kind) => name -> expr
  }.toMap
  require(aliases.keySet.intersect(foreigns.keySet).isEmpty, "there are ambiguous definitions")

  def alias(name: String): Option[AliasExpr] =
    aliases.get(name)

  def foreign(name: String): Option[ForeignExpr] =
    foreigns.get(name)
}

trait ChuskyParser {

  def parse(seq: Seq[ChuskyToken]): Either[PrettyPacker.PackerException, ModuleExpr]
}

final class DefaultChuskyParser extends ChuskyParser {
  type Pckr[Out] = Packer[Vector[ChuskyToken], ChuskyToken, Out]

  private val syntax = new VectorPackerSyntax[ChuskyToken]
  import syntax._

  lazy val nameExpr: Pckr[String] = fromPartial {
    case Name(v) => v
  }

  lazy val kindedNameExpr: Pckr[KindedName] = (nameExpr ~ P(Colon) ~ kindExpr).map {
    case (name, kind) => KindedName(name, kind)
  }
  lazy val textExpr: Pckr[String] = fromPartial {
    case Text(v) => v
  }
  lazy val importExpr: Pckr[ImportExpr] = (P(Import) ~ fromPartial {
    case Text(v) => v
  }).map(ImportExpr)
  lazy val kindGroup: Pckr[KindExpr] = P(Parenthesis.Open) ~ kindExpr ~ P(Parenthesis.Close)
  lazy val kindExpr: Pckr[KindExpr] =
    ((kindGroup | P(Star).as(KindExpr.Star)) ~ (P(Arrow) ~ kindExpr).?).map {
      case (from, maybeTo) =>
        maybeTo
          .map { to =>
            Fun(from, to)
          }
          .getOrElse(from)
    }
  lazy val foreignExpr: Pckr[ForeignExpr] = (P(Foreign) ~ kindedNameExpr).map { name =>
    ForeignExpr(name.name, name.kind)
  }
  lazy val aliasExpr: Pckr[AliasExpr] =
    (P(Alias) ~ nameExpr ~ (P(Colon) ~ kindExpr).? ~ P(Equals) ~ typeExpr).map {
      case (name, maybeKind, definition) => AliasExpr(name, maybeKind, definition)
    }

  lazy val typeGroup: Pckr[TypeExpr]                      = P(Parenthesis.Open) ~ typeExpr ~ P(Parenthesis.Close)
  lazy val taggedOption: Pckr[(String, Option[TypeExpr])] = nameExpr ~ (P(Equals) ~ typeExpr).?
  lazy val unionExr: Pckr[UnionExpr] =
    (P(Union) ~ P(Bracket.Open) ~ typeExpr.rep(1, sep = P(Comma)) ~ P(Bracket.Close))
      .map { options =>
        UnionExpr(options)
      }
  lazy val taggedUnionExpr: Pckr[TaggedUnionExpr] =
    (P(Union) ~ P(Curly.Open) ~ taggedOption.rep(1, sep = P(Comma)) ~ P(Curly.Close))
      .map(_.toMap)
      .map { options =>
        TaggedUnionExpr(options)
      }

  lazy val funExpr: Pckr[LambdaExpr] =
    (P(Parenthesis.Open) ~ kindedNameExpr.rep(1, sep = P(Comma)) ~ P(Parenthesis.Close) ~ P(
      Arrow
    ) ~ typeExpr).map {
      case (args, body) =>
        args
          .foldRight(body) { (name, expr) =>
            LambdaExpr(name, expr)
          }
          .asInstanceOf[LambdaExpr]
    }

  lazy val productExpr: Pckr[ProductExpr] =
    (P(Product) ~ P(Bracket.Open) ~ typeExpr.rep(sep = P(Comma)) ~ P(Bracket.Close))
      .map { args =>
        ProductExpr(args)
      }
  lazy val recordExpr: Pckr[RecordExpr] =
    (P(Product) ~ P(Bracket.Open) ~ (nameExpr ~ P(Equals) ~ typeExpr).rep(sep = P(Comma)) ~ P(
      Bracket.Close
    )).map { args =>
      RecordExpr(args.toMap)
    }

  lazy val delimitedFunExpr =
    (P(Curly.Open) ~ kindedNameExpr.rep(1, sep = P(Comma)) ~ P(Arrow) ~ typeExpr ~ P(Curly.Close))
      .map {
        case (args, body) =>
          args
            .foldRight(body) { (name, expr) =>
              LambdaExpr(name, expr)
            }
            .asInstanceOf[LambdaExpr]
      }

  /**
    * ubateResponse of (seq of config_requests)
    */
  lazy val delimitedTypeExpr: Pckr[TypeExpr] =
    typeGroup | taggedUnionExpr | unionExr | refExpr | delimitedFunExpr | productExpr | recordExpr
  lazy val applyExpr: Pckr[ApplyExpr] =
    (delimitedTypeExpr ~ P(Parenthesis.Open) ~ typeExpr.rep(1, sep = P(Comma)) ~ P(
      Parenthesis.Close
    )).map {
      case (fn, input) =>
        input
          .foldLeft(fn) { (expr, input) =>
            ApplyExpr(expr, input)
          }
          .asInstanceOf[ApplyExpr]
    }
  lazy val refExpr: Pckr[RefExpr]   = nameExpr.map(n => RefExpr(n))
  lazy val typeExpr: Pckr[TypeExpr] = delimitedTypeExpr | funExpr | applyExpr
  lazy val moduleExpr: Pckr[ModuleExpr] =
    ((importExpr ~ P(Semicolon)).rep() ~ ((aliasExpr | foreignExpr) ~ P(Semicolon)).rep())
      .map {
        case (imports, statements) =>
          ModuleExpr(imports, statements)

      }

  lazy val finalExpr    = moduleExpr ~ End
  lazy val prettyPacker = PrettyPacker.version1(finalExpr)
  def parse(seq: Seq[ChuskyToken]): Either[PrettyPacker.PackerException, ModuleExpr] = {
    prettyPacker.process(seq.toVector)
  }
}
