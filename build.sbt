import sbt.internal.inc.classpath.ClasspathUtilities

val CirceVersion   = "0.13.0"
val LogbackVersion = "1.2.3"
val ZioVersion     = "1.0.4"

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
    scalaVersion := "2.13.1",
    addCompilerPlugin("org.typelevel" %% "kind-projector"     % "0.10.3"),
    addCompilerPlugin("com.olegpy"    %% "better-monadic-for" % "0.3.0"),
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding",
      "UTF-8",
      "-language:higherKinds",
      "-language:postfixOps",
      "-feature",
      "-Xfatal-warnings"
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
      "com.propensive" %% "magnolia" % "0.17.0",
      // "org.scala-lang"              % "scala-reflect"   % scalaVersion.value % Provided,
      "dev.zio"                    %% "zio-test-sbt"    % ZioVersion % Test,
      "dev.zio"                    %% "zio"             % ZioVersion,
      "io.circe"                   %% "circe-parser"    % "0.13.0",
      "ch.qos.logback"              % "logback-classic" % LogbackVersion,
      "com.typesafe.scala-logging" %% "scala-logging"   % "3.9.2",
      "com.lihaoyi"                %% "pprint"          % "0.5.6"    % Test
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
      "io.circe"                   %% "circe-parser"    % "0.13.0",
      "ch.qos.logback"              % "logback-classic" % LogbackVersion,
      "com.typesafe.scala-logging" %% "scala-logging"   % "3.9.2",
      "com.lihaoyi"                %% "pprint"          % "0.5.6"    % Test,
      "io.suzaku"                  %% "boopickle"       % "1.3.3"
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
      "io.circe"                   %% "circe-parser"    % "0.13.0",
      "ch.qos.logback"              % "logback-classic" % LogbackVersion,
      "com.typesafe.scala-logging" %% "scala-logging"   % "3.9.2",
      "com.lihaoyi"                %% "pprint"          % "0.5.6"    % Test,
      "io.suzaku"                  %% "boopickle"       % "1.3.3"
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
      "io.circe"                   %% "circe-core"    % "0.13.0",
      "com.jsoniter"                % "jsoniter"      % "0.9.1",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"
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
