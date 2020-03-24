val Http4sVersion  = "0.21.1"
val CirceVersion   = "0.13.0"
val Specs2Version  = "4.8.3"
val LogbackVersion = "1.2.3"

resolvers += "jitpack" at "https://jitpack.io"

lazy val root = (project in file("."))
  .settings(
    organization := "com.github.caeus",
    name := "elodin",
    version := "0.0.1-SNAPSHOT",
    scalaVersion := "2.13.1",
    libraryDependencies ++= Seq(
      //"com.github.caeus" % "plutus"          % "2a03167479",
      "dev.zio"        %% "zio-test"       % "1.0.0-RC18-2" % Test,
      "dev.zio"        %% "zio-test-sbt"   % "1.0.0-RC18-2" % Test,
      "dev.zio"        %% "zio"            % "1.0.0-RC18-2",
      "ch.qos.logback" % "logback-classic" % LogbackVersion
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
