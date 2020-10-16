package io.github.caeus.elodin.grammar

import io.github.caeus.elodin.compile.Node.{AppNode, RefNode}
import io.github.caeus.elodin.compile.{CompileError, DefaultLexer, DefaultParser, ElodinToken, Node}
import io.github.caeus.plutus.PackerSyntax.VectorPackerSyntax
import io.github.caeus.plutus.{Packer, PrettyPacker}
import zio.test.Assertion.{anything, equalTo, isSubtype}
import zio.test._
import zio.test.environment._
import zio.{IO, Task, ZIO}

object ElodinGrammarSuites extends DefaultRunnableSpec {
  val lexer  = new DefaultLexer
  val parser = new DefaultParser

  def parse(code: String): IO[CompileError, Node] =
    for {
      tokens <- lexer.lex(code)
      node   <- parser.parse(tokens)
    } yield node

  def parseWith[Out](
      code: String,
      packer: Packer[Vector[ElodinToken], ElodinToken, Out]
  ): IO[CompileError, Out] = {
    val syntax = new VectorPackerSyntax[ElodinToken]
    import syntax._
    for {
      tokens <- lexer.lex(code)
      node <- ZIO
               .fromEither(PrettyPacker.version1(packer).process(tokens))
               .mapError(e => CompileError(e.getMessage, None))
    } yield node
  }

  def cprint(any: Any) = println(pprint.tokenize(any, indent = 2, width = 10).mkString(""))
  override def spec = {
    suite("Elodin Grammar")(
      testM("Full features") {
        assertM(parse("""
            |import "math"^{pepe,asd} Math{sin=seno};
            |import "qwe"^{};
            |{
            |  do
            |    arbitraryList = [a,b,c,d];
            |    firstActionR <- WhateverEffect;
            |    let
            |      sum = (list) => if (list >>> isEmpty) 0 { let
            |       head = listHead(list);
            |       tail = listTail(list);
            |       plus(head,sum(tail))
            |      };
            |      (+++) = 5;
            |    sum [1,2,x]
            |} EffectChain
            |""".stripMargin)) {
          anything
        }
      },
      testM("do expression simple") {
        assertM(
          parseWith(
            """
              |do
              | println("What's your name?");
              | name <- readline;
              | (+++) = concat["Hello ", name, "! how are you?"];
              | println((+++));
              | "eff"\(***)
              |""".stripMargin,
            parser.doExpr
          )
        ) {
          Assertion
            .assertion("whatever")() { node =>
              true
            }
        }
      },
      testM("apply simple") {
        assertM(parseWith("sum [1,2,3]", parser.applyExpr)) {
          isSubtype[Node.AppNode](anything)
        }
      },
      testM("apply operands (left associative)") {
        val value = parseWith("list >>> isEmpty >>> whatever", parser.applyExpr)
        assertM(value)(
          isSubtype[AppNode](
            equalTo(
              AppNode(
                Seq(
                  RefNode(">>>"),
                  AppNode(
                    Seq(
                      RefNode(">>>"),
                      RefNode("list"),
                      RefNode("isEmpty")
                    )
                  ),
                  RefNode("whatever")
                )
              )
            )
          )
        )
      },
      testM("apply operands (right associative)") {
        val value = parseWith("list >>>: isEmpty >>>: whatever", parser.applyExpr)
        assertM(value)(
          isSubtype[AppNode](
            equalTo(
              AppNode(
                Seq(
                  RefNode(">>>:"),
                  RefNode("list"),
                  AppNode(
                    Seq(
                      RefNode(">>>:"),
                      RefNode("isEmpty"),
                      RefNode("whatever")
                    )
                  )
                )
              )
            )
          )
        )
      },
      testM("apply operands complex") {
        val value = parseWith("list(1,b,c)1 3 >>>: isEmpty >>>: whatever", parser.applyExpr)
        assertM(value)(
          anything
        )
      },
      testM("negative integer") {
        val value = parseWith("-123", parser.intLiteral)
        assertM(value)(
          anything
        )
      },
      testM("complex") {
        assertM(
          parseWith(
            """(list) => (if (isEmpty(list)) 0 {let
              |   head = listHead(list);
              |   tail = listTail(list);
              |   plus(head,sum(tail))
              |  })""".stripMargin,
            parser.funExpr
          )
        )(
          isSubtype[Node](anything)
        )
      }
    )

  }
}
