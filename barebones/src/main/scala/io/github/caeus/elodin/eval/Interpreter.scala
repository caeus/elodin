package io.github.caeus.elodin.eval

import io.github.caeus.elodin.compile.BundleReader
import io.github.caeus.elodin.compile.Compiler
import io.github.caeus.elodin.eval.Eval.Kernel
import io.github.caeus.elodin.util.LilThrowable
import io.github.caeus.elodin.value.Value
import zio.*

trait Interpreter {
  def run(namespace: String, code: String): Task[Value]

  def resolve[E, O](zio: ZIO[Kernel, E, O]): IO[E, O]
}

final class LiveInterpreter(
    compiler: Compiler,
    archive: Ref.Synchronized[Archive],
    eni: ENI
) extends Interpreter {
  override def run(namespace: String, code: String): Task[Value] = {
    (for {
      compiled <- compiler.compile(namespace, code)
      _ <- archive
            .updateZIO(_.child(compiled))
      r <- archive.get.flatMap(_.module(namespace))
    } yield r).provideEnvironment(ZEnvironment(eni))
  }

  override def resolve[E, O](zio: ZIO[Kernel, E, O]): IO[E, O] = {
    archive.get.flatMap { archive =>
      zio.provideEnvironment(ZEnvironment(archive,eni))
    }
  }
}
object LiveInterpreter{
  def barebones: UIO[Interpreter] = {
    val compiler = Compiler.make
    val eni = LiveENI.make
    for {
      archive <- LiveArchive.barebones
      archive <- Ref.Synchronized.make(archive: Archive)
    } yield new LiveInterpreter(compiler, archive, eni)
  }
}

