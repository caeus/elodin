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
      "dev.zio"                    %% "zio-test"        % "1.0.0-RC18-2" % Test,
      "dev.zio"                    %% "zio-test-sbt"    % "1.0.0-RC18-2" % Test,
      "dev.zio"                    %% "zio"             % "1.0.0-RC18-2",
      "io.circe"                   %% "circe-core"      % "0.13.0",
      "com.jsoniter"                % "jsoniter"        % "0.9.1",
      "ch.qos.logback"              % "logback-classic" % LogbackVersion,
      "org.scala-lang"              % "scala-compiler"  % "2.13.1",
      "com.typesafe.scala-logging" %% "scala-logging"   % "3.9.2",
      "org.codehaus.groovy"         % "groovy"          % "3.0.3"
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
