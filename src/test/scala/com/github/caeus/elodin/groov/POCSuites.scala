package com.github.caeus.elodin.groov

import java.io.{File, FileOutputStream, ObjectOutputStream}

import groovy.lang.{Closure, GroovyClassLoader}
import zio.test.Assertion._
import zio.test._
import zio.{Task, ZManaged}

import scala.jdk.CollectionConverters

object ASD {
  def writeObject(file: File)(anyRef: AnyRef): Task[Unit] = {
    ZManaged
      .make(
        Task {
          val fos = new FileOutputStream(file)
          new ObjectOutputStream(fos)
        }
      )(oos => Task(oos.close()).either)
      .use { oos =>
        Task(oos.writeObject(anyRef))
      }
  }
  def writeByteArray(file: File)(bytes: Array[Byte]): Task[Unit] = {
    ZManaged
      .make(
        Task {
          new FileOutputStream(file)
        }
      )(oos => Task(oos.close()).either)
      .use { oos =>
        Task(oos.write(bytes))
      }
  }
}

object POCSuites extends DefaultRunnableSpec {
  override def spec =
    suite("prove")(
      suite("of")(testM("concept") {
        import org.codehaus.groovy.control.CompilationUnit
        val cu = new CompilationUnit
        cu.addSource(
          "ClosureFactory.groovy",
          """
            |package asd.asd;
            |public class ClosureFactory implements scala.Function0<Closure> { public Closure apply() { return {mssage -> println mssage} }; }
            |""".stripMargin
        )
        cu.compile()
        import CollectionConverters._
        val classLoader = new GroovyClassLoader()

        cu.getClasses.asScala.toList.foreach { cl =>
          println(cl.getName)

          classLoader.defineClass(cl.getName, cl.getBytes)
        }
        def deleteFolder(file: File):Unit = {
          println(s"OHHHHHH////$file")
          if (file.isDirectory) {
            file.listFiles().foreach(deleteFolder)
          }
          file.delete()
        }
        deleteFolder(new File("asd"))
        classLoader
          .loadClass("asd.asd.ClosureFactory")
          .newInstance()
          .asInstanceOf[Function0[Closure[_]]]
          .apply()
          .asInstanceOf[Closure[_]]
          .call("HOLA")
        assertM(for {
          _ <- Task.unit
        } yield ())(anything)
      })
    )
}
