package io.github.caeus.elodin.archive

import io.github.caeus.elodin.core.{Book, EvalError, Thunk, Val, ValRef}
import io.github.caeus.elodin.compile.Shifter
import io.github.caeus.elodin.runtime.RTError
import io.github.caeus.elodin.{ElodinEval, ElodinRT, archive}
import zio.ZIO

final case class Effect(
    arity: Int,
    cast: List[ValRef] => ZIO[ElodinEval, EvalError, Val],
    perform: List[Val] => ZIO[ElodinRT, RTError, Val]
)

/**
  * Modules that come out of sourcecode, contrary to native modules which contain references to host Language
  */
case class NBook(
    title: String,
    export: Map[String, String],
    thunks: Map[String, Thunk]
) extends Book {
  def exported: Set[String] = export.keySet

  override def thunk(id: String): Option[Thunk] =
    if (`export`.contains(id)) thunks.get(`export`(id))
    else thunks.get(id)
}
case class CBook(title: String, export: Map[String, String], shifts: Map[String, Shifter])
    extends Book {
  override lazy val exported: Set[String] = export.keySet

  override def thunk(id: String): Option[Thunk] = {
    (if (exported.contains(id)) shifts.get(`export`(id))
     else shifts.get(id))
      .map { shift =>
        Thunk(
          shift.arity,
          { args =>
            for {
              eval <- ZIO.environment[ElodinEval]
              ref  <- shift.shift.apply(args)
              r    <- ref.memoEval(eval)
            } yield r
          }
        )
      }

  }

  override def toString: String =
    s"""$exported
         |
         |${shifts.mkString("\n")}
         |""".stripMargin

}
