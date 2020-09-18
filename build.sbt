val Http4sVersion  = "0.21.1"
val CirceVersion   = "0.13.0"
val Specs2Version  = "4.8.3"
val LogbackVersion = "1.2.3"

inThisBuild(
  List(
    organization := "io.github.caeus",
    developers := List(
      Developer(
        "caeus",
        "Alejandro Navas",
        "camilo.a.navas@gmail.com",
        url("https://github.com/caeus")
      )
    ),
    licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    scalaVersion := "2.13.1"
  )
)

lazy val root = project
  .in(file("."))
  .settings(
    name := "elodin",
    skip in publish := true
  )
  .aggregate(core, plutus)

lazy val plutus = (project in file("plutus"))
  .settings(
    name := "plutus",
    version := "0.0.1-SNAPSHOT",
    scalaVersion := "2.13.1",
    homepage := Some(url("https://github.com/caeus/elodin")),
    libraryDependencies ++= Seq(
      "dev.zio"                    %% "zio-test-sbt"  % "1.0.0" % Test,
      "io.circe"                   %% "circe-core"    % "0.13.0",
      "com.jsoniter"                % "jsoniter"      % "0.9.1",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"
    ),
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    addCompilerPlugin("org.typelevel" %% "kind-projector"     % "0.10.3"),
    addCompilerPlugin("com.olegpy"    %% "better-monadic-for" % "0.3.0")
  )
lazy val core = (project in file("core"))
  .dependsOn(plutus)
  .settings(
    name := "elodin-core",
    version := "0.0.1-SNAPSHOT",
    scalaVersion := "2.13.1",
    homepage := Some(url("https://github.com/caeus/elodin")),
    libraryDependencies ++= Seq(
      "dev.zio"                    %% "zio-test-sbt"    % "1.0.0" % Test,
      "dev.zio"                    %% "zio"             % "1.0.0",
      "io.circe"                   %% "circe-parser"    % "0.13.0",
      "ch.qos.logback"              % "logback-classic" % LogbackVersion,
      "com.typesafe.scala-logging" %% "scala-logging"   % "3.9.2",
      "com.lihaoyi"                %% "pprint"          % "0.5.6" % Test
    ),
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    addCompilerPlugin("org.typelevel" %% "kind-projector"     % "0.10.3"),
    addCompilerPlugin("com.olegpy"    %% "better-monadic-for" % "0.3.0")
  )

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding",
  "UTF-8",
  "-language:higherKinds",
  "-language:postfixOps",
  "-feature",
  "-Xfatal-warnings"
)
