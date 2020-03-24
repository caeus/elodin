package com.github.caeus.elodin.interpreter

import com.github.caeus.elodin.interpreter.RefResolution.{IsNode, IsParam, Undefined}
import com.github.caeus.elodin.interpreter.Scope.{Applied, Branch}
import com.github.caeus.elodin.interpreter.Val.App
import com.github.caeus.elodin.lang.Node.{LambdaNode, LetNode}
import com.github.caeus.elodin.lang.{Node, Path, Step}

sealed trait Impl {
  def applyTo(args: Val*): Impl
}
object Impl {
  case class Native(name: String, args: Seq[Val]) extends Impl {
    override def applyTo(args: Val*): Impl = Native(name, this.args.appendedAll(args))
  }
  case class Virtual(scope: Scope) extends Impl {
    override def applyTo(args: Val*): Impl = Virtual(scope.applyTo(args: _*))
  }

}
object ArgsOf {
  object Virtual {
    def unapply(value: Val): Option[(Scope, Seq[Val])] = {
      value match {
        case App(Impl.Virtual(scope)) =>
          scope match {
            case Applied(_, _args) => Some(scope -> _args)
            case _                 => Some(scope -> Nil)
          }
        case _ => None
      }
    }
  }
  object Native {
    def unapply(value: Val): Option[(String, Seq[Val])] = {
      value match {
        case App(Impl.Native(name, args)) =>
          Some(name -> args)
        case _ => None
      }
    }
  }
}
sealed trait RefResolution
object RefResolution {
  case class IsNode(scope: Scope) extends RefResolution
  case class IsParam(value: Val)  extends RefResolution
  case object Undefined           extends RefResolution
}
object NodeOf {
  def unapply(scope: Scope): Option[(Scope, Node)] = {
    Some(scope -> scope.node)
  }
}
sealed trait Scope {
  def module: String
  def steps: List[Step]
  def node: Node
  final def path                      = Path(steps)
  final def /(step: Step): Scope      = Branch(step, this)
  final def down: Scope               = /(Step.Down)
  final def key(value: String): Scope = /(Step.Key(value))
  final def index(value: Int): Scope  = /(Step.Index(value))

  final def args: Option[Seq[Val]] = this match {
    case Applied(_, args) => Some(args)
    case _                => None
  }
  final def manyKeys(set: Set[String]): Map[String, Scope] = {
    set.toSeq.map { key =>
      (key, this.key(key))
    }.toMap
  }
  final def manyIndexes(until: Int): Seq[Scope] = 0.until(until).map(index)

  final def applyTo(args: Val*): Scope = this match {
    case Applied(underlying, _args) => Applied(underlying, _args.appendedAll(args))
    case underlying                 => Applied(underlying, args.toSeq)
  }

  def resolveRef(to: String): RefResolution
}
object Scope {
  case class Root(module: String, node: Node) extends Scope {
    override def steps: List[Step] = Nil

    override def resolveRef(to: String): RefResolution = Undefined
  }
  case class Branch(step: Step, parent: Scope) extends Scope {
    override lazy val steps: List[Step] = step :: parent.steps

    override lazy val node: Node = parent.node / step

    override lazy val module: String = parent.module

    override def resolveRef(to: String): RefResolution = node match {
      case LetNode(bindings, _) if bindings.contains(to) =>
        IsNode(this.key(to))
      case LambdaNode(params, _) => Undefined
      case _                     => parent.resolveRef(to)
    }
  }
  case class Applied(underlying: Scope, _args: Seq[Val]) extends Scope {
    override lazy val steps: List[Step] = underlying.steps

    override lazy val node: Node = underlying.node

    override lazy val module: String = underlying.module

    override def resolveRef(to: String): RefResolution = node match {
      case LambdaNode(params, _) if params.contains(to) =>
        val index = params.lastIndexOf(to)
        if (index >= 0) {
          IsParam(_args(index))
        } else Undefined
      case _ => ???
    }
  }
}
