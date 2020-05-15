package com.github.caeus.elodin.modules

import com.github.caeus.elodin.modules
import com.github.caeus.elodin.modules.ModuleInst.{DefFolder, Multi}
import com.github.caeus.elodin.modules.ModuleOp.DefMember
import com.github.caeus.elodin.compiler.Shifter
import com.github.caeus.elodin.runtime.Val.FnPointer
import com.github.caeus.elodin.runtime.{EloSystem, PPointer, Val}
import zio.{RIO, Task}



case class EloFolder(arity: Int, impl: Seq[Val] => RIO[EloSystem, Val])
object EloFolder
trait EloModule {
  def namespace: String
  def apply(member: String): Task[Val]
}
final class NativeModule(val namespace: String, functions: Map[String, EloFolder])
    extends EloModule {

  def folder(id: String): Task[EloFolder] = Task.effect(functions(id))
  override def apply(member: String): Task[Val] =
    Task.effect {
      val _ = functions(member)
      Val.Lazy(PPointer.Native(namespace, member), Nil)
    }

}
object NativeModule {
  private final def buildRec(inst: ModuleInst, result: Map[String, EloFolder]): Map[String, EloFolder] = {
    inst match {
      case DefFolder(name, folder) =>
        result.updated(name, folder)
      case Multi(insts) =>
        insts.foldLeft(result) { (result_, inst) =>
          buildRec(inst, result_)
        }
    }
  }
  def apply(name: String)(b: ModuleBuilder => ModuleInst): EloModule = {
    new NativeModule(name, buildRec(b(new ModuleBuilder(name)), Map.empty))
  }
}
final class SrcModule(
    val namespace: String,
    shifters: IndexedSeq[Shifter],
    members: Task[Map[String, Val]]
) extends EloModule {

  def folder(id: Int): Task[EloFolder] =
    Task.effect {
      val shifter = shifters(id)
      modules.EloFolder(shifter.arity, shifter.shift.apply)
    }
  override def apply(member: String): Task[Val] =
    members.flatMap { members =>
      Task.effect(members(member))
    }
}

object SrcModule {

  private def buildRec(value: Val, members: Map[String, Val]): RIO[EloSystem, Map[String, Val]] = {
    RIO.environment[EloSystem].flatMap { system =>
      system.atomize(value).flatMap {
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
  def build(namespace: String, shifters: IndexedSeq[Shifter]): RIO[EloSystem, SrcModule] = {
    val initialInstruction = shifters(0)
    if (initialInstruction.arity > 0)
      Task.fail(new Exception("This makes no fucking sense"))
    else {
      RIO.environment[EloSystem].flatMap { system =>
        initialInstruction
          .shift(Nil)
          .flatMap(value => buildRec(value, Map.empty))
          .provide(system)
          .memoize
          .map { members =>
            new SrcModule(namespace, shifters, members)
          }
      }
    }
  }
}
