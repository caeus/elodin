package com.github.caeus.elodin.asd.asd.asd

import com.github.caeus.elodin.lang.Node
import com.github.caeus.elodin.lang.Node._
import io.circe.Json

object Hola {
  trait Env {
    def ref(to: String): Kal
    def enrichWith(bindings: Map[String, Kal]): Env
  }

  class LetEnv(env: Env => Map[String, Kal]) extends Env {
    override def ref(to: String): Kal = ???

    override def enrichWith(bindings: Map[String, Kal]): Env = ???
  }
  trait Kal
  trait Kalom extends Kal

  type Noode = Env => Kal

  def $lazy(skope: Env, pepe: Noode): Kal = new $Lazy(skope, pepe)

  def $unit: Kalom

  class $Lazy(skope: Env, noode: Noode) extends Kal
  case class $Ref(to: String) extends Noode {
    override def apply(env: Env): Kal = env.ref(to)
  }

  case class $Fn(env: Env, params: Seq[String], args: Seq[Kal], body: Noode) extends Noode {
    require(args.size <= params.size)

    override def apply(v1: Env): Kal = {
      if (args.size == params.size) {
        body(env.enrichWith(params.zip(args).toMap))
      } else
        new Fn$Kal(env, params, args, body)
    }
  }

  case class Fn$Kal(skope: Env, params: Seq[String], args: Seq[Kal], body: Noode) extends Kal {
    require(args.size < params.size)
  }
  class $Int(value: String) extends Noode {
    override def apply(v1: Env): Kal = Int$Kal(BigInt(value))
  }
  case class Int$Kal(value: BigInt) extends Kalom

  class $Float(value: String) extends Noode {
    override def apply(v1: Env): Kal = Float$Kal(BigDecimal(value))
  }
  case class Float$Kal(value: BigDecimal) extends Kalom

  def kalomize(kal: Kal): Kalom = ???

  object Applicable {
    def unapply(arg: Kal): Option[(Int, Seq[Kal] => Kal)] = ???
  }

  def aaaaply(args: List[Kal]): Kalom = {
    args match {
      case Nil          => $unit
      case value :: Nil => kalomize(value)
      case (partial @ Applicable(arity, kalcu)) :: args =>
        val (taken, remnant) = args.splitAt(arity)
        aaaaply(kalcu(taken) :: remnant)
      case _ => ???
    }
  }
  class $Let(bindings: Env => Map[String, Noode], body: Noode) extends Noode {
    override def apply(env: Env): Kal = {
      body(new LetEnv({ env =>
        bindings(env).view.mapValues { asd =>
          $lazy(env, asd)
        }.toMap
      }))
    }
  }
  class $Apply(args: List[Noode]) extends Noode {
    override def apply(skope: Env): Kal =
      args match {
        case Nil          => $unit
        case noode :: Nil => $lazy(skope, noode)
        case $Fn(skope, params, preArgs, body) :: args =>
          ???
      }
  }

  def $int(value: String): Noode   = ???
  def $float(value: String): Noode = { _ => ??? }
  def $text(value: String): Noode  = { _ => ??? }
  def $apply(args: Seq[Noode]): Noode = { $ =>
    args.size match {
      case 0 => ??? //unit
      case 1 =>
        args.head($) match {
          case $Fn(params, preArgs, body) =>
            val (fnArgs, remaining) = preArgs.appendedAll(args.tail.map($lazy)).splitAt(params.size)
            $apply(remaining.prepended($Fn(params, fnArgs, body)))
            ???
        }
      case _ =>
    }
  }
  def $ref(to: String): Noode                                = { $ => $.ref(to) }
  def $fn(params: Seq[String], body: Noode): Noode           = ???
  def $let(bindings: Map[String, Noode], body: Noode): Noode = ???
  def $req(to: String): Noode                                = ???
  def $dict(items: Map[String, Noode]): Noode                = ???
  def $arr(items: Seq[Noode]): Noode                         = ???

  def escape(str: String) = Json.fromString(str).noSpaces

  def let$code(letNode: LetNode): String = {
    val bindings = letNode.bindings.toSeq
      .map {
        case (key, value) =>
          s"""$$$$${escape(key)} : ${toCode(value)}"""
      }
      .mkString("[", "\n", "]")
    s"""
      |$$let($$,{$$ -> $bindings},{$$-> ${toCode(letNode.body)})
    """.stripMargin
  }

  def ref$code(refNode: RefNode): String = s"""$$.ref{}"""

  def req$code(reqNode: ReqNode): String = s"""$$req($$,${escape(reqNode.to)})"""

  def text$code(textNode: TextNode): String = escape()

  def dict$code(dictNode: DictNode): String = {
    if (dictNode.items.isEmpty) {
      s"""$$dict([:])"""
    } else {
      val items = dictNode.items.toSeq
        .map {
          case (key, value) =>
            s"""$$$$${escape(key)} : ${toCode(value)}"""
        }
        .mkString("[", "\n", "]")
      s"""$$dict($items)"""
    }
  }
  def arr$node(arrNode: ArrNode): String = {

    val items = arrNode.items.toSeq
      .map(toCode)
      .mkString("[", "\n", "]")
    s"""$$arr($items)"""

  }

  def fn$code(fnNode: FnNode) = {
    val params = fnNode.params.map(Json.fromString).map(_.noSpaces).mkString("[", ",", "]")
    s"""$$fn($$,$params,{$$ -> ${toCode(fnNode.body)} })"""
  }
  def apply$code(applyNode: ApplyNode) = {
    val args = applyNode.args.map(toCode).mkString(", ")
    s"""$$apply($args)"""
  }
  def toCode(node: Node): String = {
    node match {
      case letNode: LetNode     => let$code(letNode)
      case fnNode: FnNode       => fn$code(fnNode)
      case applyNode: ApplyNode => apply$code(applyNode)
      case refNode: RefNode     =>
    }
  }

}
