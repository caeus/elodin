package aslkdj.qwlekj

import aslkdj.qwlekj.Trail.{Child, Root}
import com.github.caeus.elodin.interpreter.Val
import com.github.caeus.elodin.lang.Node
import com.github.caeus.elodin.lang.Node.{
  AppNode,
  ArrNode,
  BoolNode,
  DictNode,
  IntNode,
  LambdaNode,
  LetNode,
  RealNode,
  RefNode,
  ReqNode,
  StrNode
}

sealed trait Trail[+N <: Node] {
  def node: N
}
object Trail {
  case class Root[+N <: Node](node: N) extends Trail[N]
  case class Child[+N <: Node, PN <: Node](parent: Scope[PN], nest: PN => N) extends Trail[N] {
    override lazy val node: N = nest(parent.node)
  }
}
sealed abstract class Scope[+N <: Node](trail: Trail[N]) {
  final def node: N = trail.node
}

object Scope {
  case class LetScope(trail: Trail[LetNode]) extends Scope[LetNode](trail) {

    def in: Scope[Node] = createChild(this)(_.body)

    def binding(name: String): Option[Scope[Node]] =
      if (trail.node.bindings.contains(name))
        Some(createChild(this)(_.bindings(name)))
      else None
  }
  case class LambdaScope(trail: Trail[LambdaNode], args: Seq[Val] = Seq.empty[Val])
      extends Scope(trail) {
    def applyTo(args: Seq[Val]): LambdaScope = new LambdaScope(trail, this.args.appendedAll(args))
    def body: Option[Scope[Node]] =
      if (trail.node.params.size >= args.size)
        Some(createChild(this)(_.body))
      else None
  }
  case class AppScope(trail: Trail[AppNode])   extends Scope(trail)
  case class RefScope(trail: Trail[RefNode])   extends Scope(trail)
  case class ArrScope(trail: Trail[ArrNode])   extends Scope(trail)
  case class DictScope(trail: Trail[DictNode]) extends Scope(trail)
  case class ReqScope(trail: Trail[ReqNode])   extends Scope(trail)
  case class StrScope(trail: Trail[StrNode])   extends Scope(trail)
  case class IntScope(trail: Trail[IntNode])   extends Scope(trail)
  case class RealScope(trail: Trail[RealNode]) extends Scope(trail)
  case class BoolScope(trail: Trail[BoolNode]) extends Scope(trail)

  def createFromTrail(trail: Trail[Node]): Scope[Node] = trail.node match {
    case _: LetNode    => LetScope(trail.asInstanceOf[Trail[LetNode]])
    case _: LambdaNode => LambdaScope(trail.asInstanceOf[Trail[LambdaNode]])
    case _: AppNode    => AppScope(trail.asInstanceOf[Trail[AppNode]])
    case _: RefNode    => RefScope(trail.asInstanceOf[Trail[RefNode]])
    case _: ArrNode    => ArrScope(trail.asInstanceOf[Trail[ArrNode]])
    case _: DictNode   => DictScope(trail.asInstanceOf[Trail[DictNode]])
    case _: ReqNode    => ReqScope(trail.asInstanceOf[Trail[ReqNode]])
    case _: StrNode    => StrScope(trail.asInstanceOf[Trail[StrNode]])
    case _: IntNode    => IntScope(trail.asInstanceOf[Trail[IntNode]])
    case _: RealNode   => RealScope(trail.asInstanceOf[Trail[RealNode]])
    case _: BoolNode   => BoolScope(trail.asInstanceOf[Trail[BoolNode]])
  }
  def createChild[PN <: Node](parent: Scope[PN])(nest: PN => Node): Scope[Node] = {
    createFromTrail(Child(parent, nest))

  }
  def createRoot(node: Node): Scope[Node] = {
    createFromTrail(Root(node))
  }
}
