
val CirceVersion   = "0.14.3"
val LogbackVersion = "1.4.1"
val ZioVersion     = "2.0.2"
val ScalaLoggingVersion = "3.9.5"
val PPrintVersion = "0.7.3"
val BoopickleVersion = "1.4.0"

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
    homepage := Some(url("https://github.com/caeus/elodin")),
    licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    scalaVersion := "3.2.0",
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding",
      "UTF-8",
      "-language:higherKinds",
      "-language:postfixOps",
      "-feature",
      "-Xfatal-warnings",
      //"-explain"
    )
  )
)

lazy val root = project
  .in(file("."))
  .settings(
    name := "elodin",
    skip in publish := true
  )
  .aggregate(plutus, core, barebones, predef)

lazy val core = project
  .in(file("core"))
  .settings(
    name := "core",
    version := "0.0.1-SNAPSHOT",
    homepage := Some(url("https://github.com/caeus/elodin")),
    libraryDependencies ++= Seq(
      "com.softwaremill.magnolia" %% "magnolia-core" % "2.0.0-M9",
      // "org.scala-lang"              % "scala-reflect"   % scalaVersion.value % Provided,
      "dev.zio"                    %% "zio-test-sbt"    % ZioVersion % Test,
      "dev.zio"                    %% "zio"             % ZioVersion,
      "io.circe"                   %% "circe-parser"    % CirceVersion,
      "ch.qos.logback"              % "logback-classic" % LogbackVersion,
      "com.typesafe.scala-logging" %% "scala-logging"   % ScalaLoggingVersion,
      "com.lihaoyi"                %% "pprint"          % PPrintVersion    % Test
    )
  )

lazy val barebones = project
  .in(file("barebones"))
  .settings(
    name := "barebones",
    version := "0.0.1-SNAPSHOT",
    libraryDependencies ++= Seq(
      "dev.zio"                    %% "zio-test-sbt"    % ZioVersion % Test,
      "dev.zio"                    %% "zio"             % ZioVersion,
      "io.circe"                   %% "circe-parser"    % CirceVersion,
      "ch.qos.logback"              % "logback-classic" % LogbackVersion,
      "com.typesafe.scala-logging" %% "scala-logging"   % ScalaLoggingVersion,
      "com.lihaoyi"                %% "pprint"          % PPrintVersion    % Test,
      "io.suzaku"                  %% "boopickle"       % BoopickleVersion
    )
  )
  .dependsOn(plutus, core)

lazy val predef = project
  .in(file("predef"))
  .settings(
    name := "predef",
    version := "0.0.1-SNAPSHOT",
    libraryDependencies ++= Seq(
      "dev.zio"                    %% "zio-test-sbt"    % ZioVersion % Test,
      "dev.zio"                    %% "zio"             % ZioVersion,
      "io.circe"                   %% "circe-parser"    % CirceVersion,
      "ch.qos.logback"              % "logback-classic" % LogbackVersion,
      "com.typesafe.scala-logging" %% "scala-logging"   % ScalaLoggingVersion,
      "com.lihaoyi"                %% "pprint"          % PPrintVersion    % Test,
      "io.suzaku"                  %% "boopickle"       % BoopickleVersion
    ),
    (resourceGenerators in Compile) += Def.task {
      val barebonesCL    = (barebones / Test / testLoader).value
      val genPredefClass = barebonesCL.loadClass("io.github.caeus.elodin.GenPredef")
      val releaseMethod  = genPredefClass.getMethod("release", classOf[String])
      val file           = (resourceManaged in Compile).value / "predef.elodinp"
      releaseMethod
        .invoke(null, file.getAbsolutePath)
      Seq(file)
    }.taskValue
  )
  .dependsOn(barebones)

lazy val plutus = (project in file("plutus"))
  .settings(
    name := "plutus",
    version := "0.0.1-SNAPSHOT",
    homepage := Some(url("https://github.com/caeus/elodin")),
    libraryDependencies ++= Seq(
      "dev.zio"                    %% "zio-test-sbt"  % ZioVersion % Test,
      "io.circe"                   %% "circe-core"    % CirceVersion,
      "com.jsoniter"                % "jsoniter"      % "0.9.23",
      "com.typesafe.scala-logging" %% "scala-logging" % ScalaLoggingVersion
    )
  )

lazy val runescript = (project in file("runescript"))
  .settings(
    name := "runescript",
    version := "0.0.1-SNAPSHOT",
    homepage := Some(url("https://github.com/caeus/elodin")),
    libraryDependencies ++= Seq(
      "dev.zio"                    %% "zio-test-sbt"  % ZioVersion % Test,
      "io.circe"                   %% "circe-core"    % CirceVersion,
      "com.jsoniter"                % "jsoniter"      % "0.9.23",
      "com.typesafe.scala-logging" %% "scala-logging" % ScalaLoggingVersion
    )
  )

def ingored = {

  lazy val chusky = (project in file("chusky"))
    .settings(
      name := "chusky",
      version := "0.0.1-SNAPSHOT",
      homepage := Some(url("https://github.com/caeus/elodin")),
      libraryDependencies ++= Seq(
        "dev.zio"                    %% "zio-test-sbt"  % ZioVersion % Test,
        "dev.zio"                    %% "zio-test"      % ZioVersion,
        "io.circe"                   %% "circe-core"    % "0.13.0",
        "com.jsoniter"                % "jsoniter"      % "0.9.1",
        "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"
      )
    )
    .dependsOn(plutus)
}
