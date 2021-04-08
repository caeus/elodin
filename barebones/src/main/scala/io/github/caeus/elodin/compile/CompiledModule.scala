package io.github.caeus.elodin.compile

import io.github.caeus.elodin.compile.Ast9.GuestModule
import io.github.caeus.elodin.util.EitherUtil

import scala.util.chaining._

sealed trait CompiledModule2 {
  def name: String
  def ast: GuestModule
}

final case class Bundle(modules: Map[String, GuestModule]) {
  def contains(module: String): Boolean = modules contains module

  def get(module: String): Option[GuestModule] = modules.get(module)

}

object Bundle {
  def empty: Bundle = Bundle(Map.empty)

  def single(at: String, module: GuestModule): Bundle = {
    Bundle(Map(at -> module))
  }

  def merge2(bundle1: Bundle, bundle2: Bundle): Either[Set[String], Bundle] = {
    val conflicts = bundle1.modules.keySet.intersect(bundle2.modules.keySet)
    if (conflicts.nonEmpty)
      Left(conflicts)
    else {
      Right(Bundle(bundle1.modules ++ bundle2.modules))
    }
  }
  def merge(bundles: Bundle*): Either[Set[String], Bundle] = {
    EitherUtil.reduceLeft(bundles) { (b0, b1) =>
      merge2(b0, b1)
    }
  }
}

object CompiledModule2 {
  def make(name: String, ast: GuestModule): CompiledModule2 =
    Impl(name: String, ast)
  private final case class Impl(name: String, ast: GuestModule) extends CompiledModule2
}
