package com.github.caeus.elodin.interpreter

sealed trait Step
object Step {
  case object Down extends Step {
    override def toString: String = "_"
  }
  case class Index(value: Int) extends Step {
    override def toString: String = value.toString
  }
  case class Key(value: String) extends Step {
    override def toString: String = value
  }
  case class FnBody(args: Map[String, Val]) extends Step {
    override def toString: String = args.mkString("[", ",", "]")
  }
}
