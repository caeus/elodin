package io.github.caeus.elodin.eval

import io.github.caeus.elodin.compile.Compiler
import io.github.caeus.elodin.eval.Eval.Kernel
import io.github.caeus.elodin.util.LilThrowable
import io.github.caeus.elodin.value.Value
import zio._

object Interpreter {

  type Box = Has[Service]
  trait Service {
    def run(namespace: String, code: String): Task[Value]

    def resolve[E, O](zio: ZIO[Kernel, E, O]): IO[E, O]
  }
  private final class LiveService(
      compiler: Compiler.Service,
      archive: RefM[Archive.Service],
      eni: ENI.Service
  ) extends Service {
    override def run(namespace: String, code: String): Task[Value] = {
      (for {
        compiled <- compiler.compile(namespace, code)
        _ <- archive
              .update(_.child(compiled))
        r <- archive.get.flatMap(_.module(namespace))
      } yield r).provide(Has(eni))
    }

    override def resolve[E, O](zio: ZIO[Kernel, E, O]): IO[E, O] = {
      archive.get.flatMap { archive =>
        zio.provide(Has(archive) add eni)
      }
    }
  }
  def barebones: UIO[Service] = {
    val compiler = Compiler.make
    val eni      = ENI.make
    for {
      archive <- Archive.barebones
      archive <- RefM.make(archive: Archive.Service)
    } yield new LiveService(compiler, archive, eni)
  }
}
