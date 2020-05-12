package com.github.caeus.elodin.runtime

sealed trait PPointer {
  def arity: Int
}
object PPointer {
  case class Native(module: String, arity: Int, member: String) extends PPointer
  case class Compiled(module: String, arity: Int, id: Int)      extends PPointer
}

sealed trait Val
object Val {

  sealed trait Atomic extends Val
  sealed trait FnPointer extends Val {
    def pointer: PPointer
    def args: Seq[Val]
    def applyTo(args: Seq[Val]): FnPointer
  }
  case class Lazy(override val pointer: PPointer, override val args: Seq[Val]) extends FnPointer {
    override def applyTo(args: Seq[Val]): FnPointer =
      Lazy(pointer, this.args.appendedAll(args))
  }
  case class Atom(of: Any) extends Atomic
  case class Fn(pointer: PPointer, args: Seq[Val]) extends Atomic with FnPointer {
    require(args.size < pointer.arity)
    override def applyTo(args: Seq[Val]): FnPointer =
      Lazy(pointer, this.args.appendedAll(args))
  }

}
