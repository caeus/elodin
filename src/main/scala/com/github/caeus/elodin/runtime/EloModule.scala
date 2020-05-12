package com.github.caeus.elodin.runtime

import com.github.caeus.elodin.compiler.ModuleInit
import com.github.caeus.elodin.runtime.ModuleOp.DefMember
import com.github.caeus.elodin.runtime.Val.FnPointer
import zio.{RIO, Task}

sealed trait ModuleOp
object ModuleOp {
  case class DefMember(name: String, value: Val) extends ModuleOp
}

case class ModuleEffect(op: ModuleOp, cont: FnPointer)
object DoneModule {}

case class EloFolder(arity: Int, impl: Seq[Val] => RIO[EloSystem, Val])
trait EloModule {
  def namespace: String
  def apply(member: String): Task[Val]
}
class NativeModule(val namespace: String, functions: Map[String, EloFolder]) extends EloModule {

  def folder(id: String): Task[EloFolder] = Task.effect(functions(id))
  override def apply(member: String): Task[Val] =
    Task.effect {
      val folder = functions(member)
      Val.Lazy(PPointer.Native(namespace, folder.arity, member), Nil)
    }

}
object NativeModule {}
final class SrcModule(val namespace: String, init: ModuleInit, members: Map[String, Val])
    extends EloModule {
  def folder(id: Int): Task[EloFolder] =
    Task.effect {
      val shifter = init.shifters(id)
      EloFolder(shifter.arity, shifter.shift.apply)
    }
  override def apply(member: String): Task[Val] = Task.effect(members(member))
}

object SrcModule {

  private def buildRec(value: Val, members: Map[String, Val]): RIO[EloSystem, Map[String, Val]] = {
    RIO.environment[EloSystem].flatMap { system =>
      system.eval(value).flatMap {
        case Val.Atom(ModuleEffect(op, cont)) =>
          op match {
            case DefMember(name, value) =>
              buildRec(cont.applyTo(Seq(Val.Atom(()))), members.updated(name, value))
          }
        case Val.Atom(DoneModule) =>
          RIO.succeed(members)
        case _ => RIO.fail(new Exception("UNAA:SDLA:SLD"))
      }
    }
  }
  def build(namespace: String, init: ModuleInit): RIO[EloSystem, SrcModule] = {
    val initialInstruction = init.shifters(0)
    if (initialInstruction.arity > 0)
      Task.fail(new Exception("This makes no fucking sense"))
    else {
      initialInstruction
        .shift(Nil)
        .flatMap(value => buildRec(value, Map.empty))
        .map { members =>
          new SrcModule(namespace, init, members)
        }
    }
  }
}
