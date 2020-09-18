package io.github.caeus.elodin.archive

import io.github.caeus.elodin.{ElodinEval, archive}
import io.github.caeus.elodin.runtime.{Link, Value}
import zio.{RIO, ZIO}

final case class DepCalculate(arity: Int, form: List[Value] => RIO[ElodinEval, Value]) {
  def cast(args: List[Value]): RIO[ElodinEval, Value] = form(args)
}
final case class Calculate(arity: Int, form: List[Link] => ZIO[ElodinEval, Unit, Link]) {
  def cast(args: List[Link]): ZIO[ElodinEval, Unit, Link] = form(args)
}
final case class DepPerform(arity: Int, form: List[Value] => RIO[ElodinEval, Either[Value, Value]])
final case class Perform(arity: Int, form: List[Link] => ZIO[ElodinEval, Unit, Either[Link, Link]])

trait Book {
  def name: String
  def exported: Map[String, Int]
  def calculation(id: Int): Option[DepCalculate]
  def action(by: String): Option[DepPerform]
}

final case class BookPageRef(book: String, page: Int)
final case class ActionRef(book: String, name: String)
object Book {

  /**
    * Modules that come out of sourcecode, contrary to native modules which contain references to host Language
    */
  case class NBook(
      name: String,
      export: Map[String, Int],
      calculations: IndexedSeq[DepCalculate],
      actions: Map[String, DepPerform]
  ) extends Book {
    override def exported: Map[String, Int] = export

    override def calculation(page: Int): Option[DepCalculate] = calculations.lift(page)

    override def action(named: String): Option[DepPerform] = actions.get(named)
  }
  case class CBook(name: String, export: Map[String, Int], shifts: IndexedSeq[Shifter])
      extends Book {
    override lazy val exported = export

    override def calculation(id: Int): Option[DepCalculate] = {
      shifts.lift(id).map { shift =>
        archive.DepCalculate(
          shift.arity,
          { args =>
            shift.shift.apply(args)
          }
        )
      }

    }

    override def toString: String =
      s"""$exported
         |
         |${shifts.mkString("\n")}
         |""".stripMargin

    override def action(by: String): Option[DepPerform] = None
  }

}
