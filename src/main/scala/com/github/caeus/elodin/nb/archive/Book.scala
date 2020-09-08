package com.github.caeus.elodin.nb.archive

import com.github.caeus.elodin.compiler.Shifter
import com.github.caeus.elodin.nb.runtime.{Atomizer, Value}
import zio.RIO

final case class Calculate(arity: Int, form: List[Value] => RIO[Atomizer, Value]) {
  def cast(args: List[Value]): RIO[Atomizer, Value] = form(args)
}
final case class Perform(arity: Int, form: List[Value] => RIO[Atomizer, Either[Value, Value]])

trait Book {
  def name: String
  def exported: Map[String, Int]
  def calculation(id: Int): Option[Calculate]
  def action(by: String): Option[Perform]
}

final case class CalculationRef(book: String, page: Int)
final case class ActionRef(book: String, name: String)
object Book {

  /**
    * Modules that come out of sourcecode, contrary to native modules which contain references to host Language
    */
  case class NBook(
      name: String,
      export: Map[String, Int],
      calculations: IndexedSeq[Calculate],
      actions: Map[String, Perform]
  ) extends Book {
    override def exported: Map[String, Int] = export

    override def calculation(page: Int): Option[Calculate] = calculations.lift(page)

    override def action(named: String): Option[Perform] = actions.get(named)
  }
  case class CBook(name: String, export: Map[String, Int], shifts: IndexedSeq[Shifter])
      extends Book {
    override lazy val exported = export

    override def calculation(id: Int): Option[Calculate] = {
      shifts.lift(id).map { shift =>
        Calculate(
          shift.arity,
          { args =>
            shift.shift.apply(args)
          }
        )
      }

    }

    override def action(by: String): Option[Perform] = None
  }

}
