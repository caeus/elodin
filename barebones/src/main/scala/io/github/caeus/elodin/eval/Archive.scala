package io.github.caeus.elodin.eval

import io.github.caeus.elodin.compile.{Ast9, Bundle}
import io.github.caeus.elodin.value.Value
import io.github.caeus.elodin.value.Value.{FunVal, ImplRef}
import zio.{Has, RefM, UIO, ZIO}

object Archive {

  type Box = Has[Service]
  trait Service {
    def reduce(
        module: String,
        offset: Int,
        scope: Scope,
        args: Seq[Value]
    ): ZIO[ENI.Box, EvalException, Value]
    def module(module: String): ZIO[ENI.Box, EvalException, Value]
    def child(more: Bundle): UIO[Service]
  }

  def barebones: ZIO[Any, Nothing, Service] = {
    RefM.make(Map.empty[String, Value]).map { ref =>
      new LiveService(None, Bundle.empty, ref, RootScope.barebones): Service
    }
  }

  private final class LiveService(
      parent: Option[Service],
      bundle: Bundle,
      evaldModules: RefM[Map[String, Value]],
      rootScope: RootScope.Service
  ) extends Service {
    override def reduce(
        module: String,
        offset: Int,
        scope: Scope,
        args: Seq[Value]
    ): ZIO[ENI.Box, EvalException, Value] =
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
            .provideSome[ENI.Box](eni => eni.add(this: Service))
        }
        .getOrElse(EvalException.referenceError(s"module:$module"))

    override def module(
        module: String
    ): ZIO[ENI.Box, EvalException, Value] = {
      if (bundle.contains(module))
        evaldModules.modify {
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
              .provideSome[ENI.Box](env => env.add(this: Service))

        }
      else {
        parent
          .map(
            _.module(module)
          )
          .getOrElse(EvalException.referenceError(s"module: $module"))
      }
    }

    override def child(newone: Bundle): UIO[Service] = {
      RefM.make(Map.empty[String, Value]).map { evald =>
        new LiveService(Some(this), newone, evald, rootScope)
      }
    }
  }
}
