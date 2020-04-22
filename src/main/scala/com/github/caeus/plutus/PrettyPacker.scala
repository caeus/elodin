package com.github.caeus.plutus

import com.github.caeus.plutus.PackerResult.{Done, Failed}
import io.circe.Json

trait PrettyPacker[Src, +Err, +R] {

  def process(src: Src): Either[Err, R]

}

object PrettyPacker {

  final class PackerException(msg: String, val errors: Seq[PackerError]) extends Exception(msg)

  def version1[Src: Slicer, El, R](packer: Packer[Src, El, R])(
      implicit toCursor: ToCursor[Src, El]): PrettyPacker[Src, PackerException, R] =
    new ThrowablePrettyPacker[Src, El, R](packer)

  private final class ThrowablePrettyPacker[Src: Slicer, El, +R](packer: Packer[Src, El, R])(
      implicit toCursor: ToCursor[Src, El])
      extends PrettyPacker[Src, PackerException, R] {

    override def process(src: Src): Either[PackerException, R] = {
      packer.take(toCursor(src)) match {
        case Done(result, _) => Right(result)
        case Failed(errors) =>
          Left(
            new PackerException(
              Json
                .arr(
                  errors
                    .groupBy(_.pos)
                    .toSeq
                    .sortBy(-_._1)
                    .map {
                      case (pos, errors) =>
                        Json.obj(
                          "position" -> Json.fromInt(pos),
                          "details" -> Json.arr(errors.map { err =>
                            Json.obj(
                              "path" -> Json.fromString(err.path.mkString("/")),
                              "sample" -> Json.fromString(
                                Slicer[Src].slice(src)(pos, pos + 5).toString),
                              "message" -> Json.fromString(err.msg)
                            )
                          }: _*)
                        )
                    }: _*)
                .spaces2,
              errors
            ))

      }
    }
  }

}
