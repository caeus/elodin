package io.github.caeus.elodin.runtime

import io.circe.{Json, JsonNumber, JsonObject}
import io.github.caeus.elodin.ElodinEval
import io.github.caeus.elodin.archive.BookPageRef
import io.github.caeus.elodin.runtime.Piece.FunS
import zio.{IO, RefM, UIO, ZIO}

sealed trait Piece
object Piece {
  case object UnitS                                                extends Piece
  final case class TextS(value: String)                            extends Piece
  final case class IntS(value: BigInt)                             extends Piece
  final case class FloatS(value: BigDecimal)                       extends Piece
  final case class BoolS(value: Boolean)                           extends Piece
  final case class ListS(items: Seq[Link])                         extends Piece
  final case class DictS(items: Map[String, Link])                 extends Piece
  final case class FunS(page: BookPageRef, args: List[Link])       extends Piece
  final case class TaggedS(book: String, tag: String, value: Link) extends Piece
  final case class ForeignS(value: Any)                            extends Piece
}

sealed trait Link {
  def piece(atomizer: ElodinEval): IO[Unit, Piece]
}

object Link {

  /**
    * A Pointer that is from the start an strict value (None, if undefined)
    * @param value
    */
  final private class ResolvedLink(value: Option[Piece]) extends Link {
    override def piece(atomizer: ElodinEval): IO[Unit, Piece] =
      ZIO.fromOption(value).mapError(_ => ())
  }

  def fromPiece(value: Piece): Link = new ResolvedLink(Some(value))
  def undefined: Link               = new ResolvedLink(None)
  def fromApply(funS: FunS): UIO[Link] =
    RefM.make(Left(funS): Either[FunS, Option[Piece]]).map { ref =>
      new LazyLink(ref)
    }

  private def _fromJson(json: Json): Link =
    json.fold[Link](
      Link.fromPiece(Piece.UnitS),
      b => Link.fromPiece(Piece.BoolS(b)),
      (n: JsonNumber) => {
        n.toBigInt
          .map(Piece.IntS)
          .orElse(n.toBigDecimal.map(Piece.FloatS))
          .map {
            Link.fromPiece
          }
          .get
      },
      t => Link.fromPiece(Piece.TextS(t)),
      { (seq: Seq[Json]) =>
        Link.fromPiece(Piece.ListS(seq.map(_fromJson)))
      },
      (obj: JsonObject) =>
        Link.fromPiece(Piece.DictS(obj.toVector.map(x => x._1 -> _fromJson(x._2)).toMap))
    )

  def fromJson(json: Json): Link = {
    _fromJson(json)
  }

  /**
    * A pointer that can be in one of two states. It's either an non evaluated yet, application
    * Or an optional strict value
    * @param ref
    */
  final private class LazyLink(ref: RefM[Either[FunS, Option[Piece]]]) extends Link {
    override def piece(atomizer: ElodinEval): ZIO[Any, Unit, Piece] = {
      ref
        .updateSomeAndGet {
          case Left(fun) =>
            atomizer
              .atomize2(fun.page, fun.args)
              .fold(_ => Right(None), p => Right(Some(p)))
        }
        .flatMap {
          case Right(value) => ZIO.fromOption(value).mapError(_ => ())
          case _            => ZIO.die(new Exception("At this point this shouldn't happen"))
        }
    }
  }

}
