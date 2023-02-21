package io.github.caeus.elodin.eval

import io.github.caeus.elodin.compile.{Ast9, Bundle}
import io.github.caeus.elodin.value.Value
import io.github.caeus.elodin.value.Value.{FunVal, ImplRef}
import zio.{Ref, UIO, ZIO}

import scala.collection.immutable.Seq

trait Archive {
  def reduce(
      module: String,
      offset: Int,
      scope: Scope,
      args: Seq[Value]
  ): ZIO[ENI, EvalException, Value]

  def module(module: String): ZIO[ENI, EvalException, Value]

  def child(more: Bundle): UIO[Archive]
}
private final class LiveArchive(
    parent: Option[Archive],
    bundle: Bundle,
    evaldModules: Ref.Synchronized[Map[String, Value]],
    rootScope: RootScope
) extends Archive {
  override def reduce(
      module: String,
      offset: Int,
      scope: Scope,
      args: Seq[Value]
  ): ZIO[ENI, EvalException, Value] =
    bundle
      .get(module)
      .flatMap { module =>
        module.fhunks.lift(offset)
      }
      .map { fhunk =>
        Eval
          .knownArity(
            fhunk.params.size,
            args => FunVal(ImplRef.Guest(module, offset, scope), args),
            { args =>
              Ast9.eval(scope.extendWithValues(fhunk.params.zip(args).toMap))(fhunk.body)
            }
          )(args)
          .provideSomeEnvironment[ENI](eni => eni.add(this: Archive))
      }
      .getOrElse(EvalException.referenceError(s"module:$module"))

  override def module(
      module: String
  ): ZIO[ENI, EvalException, Value] = {
    if (bundle.contains(module))
      evaldModules.modifyZIO {
        case evald if evald contains module =>
          ZIO.succeed {
            evald(module) -> evald
          }
        case evaldModules =>
          rootScope
            .forModule(module)
            .flatMap { scope =>
              reduce(
                module,
                0,
                scope,
                Nil
              )
            }
            .map { evaldModule =>
              evaldModule -> evaldModules.updated(module, evaldModule)
            }
            .provideSomeEnvironment[ENI](env => env.add(this: Archive))

      }
    else {
      parent
        .map(
          _.module(module)
        )
        .getOrElse(EvalException.referenceError(s"module: $module"))
    }
  }

  override def child(newone: Bundle): UIO[Archive] = {
    Ref.Synchronized.make(Map.empty[String, Value]).map { evald =>
      new LiveArchive(Some(this), newone, evald, rootScope)
    }
  }
}
object LiveArchive {

  def barebones: ZIO[Any, Nothing, Archive] = {
    Ref.Synchronized.make(Map.empty[String, Value]).map { ref =>
      new LiveArchive(None, Bundle.empty, ref, BarebonesRootScope.live): Archive
    }
  }

}
