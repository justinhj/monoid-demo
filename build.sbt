import sbt._

lazy val monoiddemo = (project in file(".")).
  settings (
    name := "monoid-demo",
    organization := "org.justinhj",
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.12.10"
    // add other settings here
  )

/* scala versions and options */
scalaVersion := "2.12.10"

addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.0")
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.6")

// These options will be used for *all* versions.
scalacOptions ++= Seq(
  "-deprecation"
    , "-unchecked"
    , "-encoding", "UTF-8"
    , "-Xlint"
    , "-Xverify"
    , "-feature"
    ,"-Ypartial-unification"
    //  ,"-Xfatal-warnings"
    , "-language:_"
    //,"-optimise"
    //,"-Xlog-implicit-conversions"
)

javacOptions ++= Seq("-Xlint:unchecked", "-Xlint:deprecation", "-source", "1.7", "-target", "1.7")

// javaOptions in Universal ++= Seq(
//   "-J-server",
//   "-J-Xms1g -Xmx4g",
//   "-J-XX:+UseConcMarkSweepGC -XX:+CMSParallelRemarkEnabled",
//   "-J-XX:+UseCMSInitiatingOccupancyOnly -XX:CMSInitiatingOccupancyFraction=68",
//   "-J-XX:+ScavengeBeforeFullGC -XX:+CMSScavengeBeforeRemark",
//   "-J-XX:+UseGCLogFileRotation -XX:NumberOfGCLogFiles=10 -XX:GCLogFileSize=100M"
// )

val CatsVersion = "2.0.0-M1"
val CatsEffectVersion = "1.3.0"
val ScalaZVersion = "7.3.0-M29"
val ZIOVersion = "1.0.0-RC18"
val ShapelessVersion = "2.3.3"
val SttpVersion = "2.0.7"

libraryDependencies ++= Seq(
  "com.typesafe" % "config" % "1.3.1",
  // -- testing --
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.6" % Test,
  // -- Logging --
  "ch.qos.logback" % "logback-classic" % "1.1.3",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.0",
  // Cats
  "org.typelevel" %% "cats-core" % CatsVersion,
  "org.typelevel" %% "cats-free" % CatsVersion,
  "org.typelevel" %% "alleycats-core" % CatsVersion,
  //"org.typelevel" %% "cats-laws" % CatsVersion,
  "org.typelevel" %% "cats-testkit" % CatsVersion,
  "org.typelevel" %% "cats-effect" % CatsEffectVersion,
  // shapeless
  "com.chuusai" %% "shapeless" % ShapelessVersion,
  // scalaz
  "org.scalaz" %% "scalaz-core" % ScalaZVersion,
  "org.scalaz" %% "scalaz-scalacheck-binding" % ScalaZVersion,
  // Monix
  "io.monix" %% "monix" % "3.1.0",
  // ZIO
  "dev.zio" %% "zio" % ZIOVersion,
  "dev.zio" %% "zio-streams" % ZIOVersion,
  "dev.zio" %% "zio-interop-monix" % "3.1.0.0-RC1",
  "dev.zio" %% "zio-interop-cats" % "2.0.0.0-RC9",
  "dev.zio" %% "zio-config" % "1.0.0-RC16",
  "dev.zio" %% "zio-config-typesafe" % "1.0.0-RC16",
  // Sttp
  "com.softwaremill.sttp.client" %% "core" % SttpVersion,
  "com.softwaremill.sttp.client" %% "async-http-client-backend-zio" % SttpVersion,
  "com.softwaremill.sttp.client" %% "json4s" % SttpVersion,
  "org.json4s" %% "json4s-native" % "3.6.0",
  "com.softwaremill.sttp.client" %% "slf4j-backend" % SttpVersion,
  // Cassandra
  "io.getquill" %% "quill-cassandra" % "3.5.1",
  "com.datastax.oss" % "java-driver-core" % "4.3.0",
  // 47 Degs
  "com.47deg" %% "github4s" % "0.22.0",
  // type classes
  "com.github.mpilquist" %% "simulacrum" % "0.12.0",
  // Li HaoYi stack
  "com.lihaoyi" %% "upickle" % "0.9.5",
  "com.lihaoyi" % "ammonite" % "2.0.3" % "test" cross CrossVersion.full
)

//wartremoverWarnings ++= Warts.unsafe

//ivyScala := ivyScala.value map { _.copy(overrideScalaVersion = true) }

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

// scalariform
//scalariformSettings

// ScalariformKeys.preferences := ScalariformKeys.preferences.value
//   .setPreference(AlignSingleLineCaseStatements, true)
//   .setPreference(DoubleIndentClassDeclaration, true)
//   .setPreference(IndentLocalDefs, true)
//   .setPreference(IndentPackageBlocks, true)
//   .setPreference(IndentSpaces, 2)
//   .setPreference(MultilineScaladocCommentsStartOnFirstLine, false)

// ammonite repl
sourceGenerators in Test += Def.task {
  val file = (sourceManaged in Test).value / "amm.scala"
  IO.write(file, """object amm extends App { ammonite.Main().run() }""")
  Seq(file)
}.taskValue

