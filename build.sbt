val Http4sVersion = "0.20.9"
val CirceVersion = "0.11.1"
val catsParVersion = "0.2.1"
val enumeratumV = "1.5.13"
val LogbackVersion = "1.2.3"

//tests
val scalacheckMagnoliaV = "0.0.2"
val scalaTestV = "3.0.8"
val specs2V = "4.7.0"

lazy val root = (project in file("."))
  .settings(
    organization := "com.avalon",
    name := "avalon-game",
    scalaVersion := "2.12.8",
//    publish / skip := true,
    libraryDependencies ++= Seq(
      "io.chrisdavenport" %% "cats-par"              % catsParVersion,
      "io.circe"          %% "circe-generic"         % CirceVersion,
      "io.circe"          %% "circe-generic-extras"  % CirceVersion,
      "io.circe"          %% "circe-parser"          % CirceVersion,
      "com.beachape"      %% "enumeratum"            % enumeratumV,
      "com.beachape"      %% "enumeratum-circe"      % enumeratumV,
      "com.beachape"      %% "enumeratum-scalacheck" % enumeratumV % Test,
      "org.http4s"        %% "http4s-blaze-server"   % Http4sVersion,
      "org.http4s"        %% "http4s-blaze-client"   % Http4sVersion,
      "org.http4s"        %% "http4s-circe"          % Http4sVersion,
      "org.http4s"        %% "http4s-dsl"            % Http4sVersion,
//      "ch.qos.logback"    %  "logback-classic"       % LogbackVersion,
      "com.mrdziuban"     %% "scalacheck-magnolia"   % scalacheckMagnoliaV,
      "org.scalatest"     %% "scalatest"             % scalaTestV % Test,
      "org.specs2"        %% "specs2-core"           % specs2V % Test,
      "org.specs2"        %% "specs2-matcher"        % specs2V % Test,
      "org.specs2"        %% "specs2-scalacheck"     % specs2V % Test,
    ),
    addCompilerPlugin("org.typelevel" %% "kind-projector"     % "0.10.3"),
    addCompilerPlugin("com.olegpy"    %% "better-monadic-for" % "0.3.0")
  )

enablePlugins(DockerPlugin)


scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-language:higherKinds",
  "-language:postfixOps",
  "-feature",
  "-Ypartial-unification",
  "-Xfatal-warnings",
)

dockerfile in docker := {
  // The assembly task generates a fat JAR file
  val artifact: File = assembly.value
  val artifactTargetPath = s"/app/${artifact.name}"

  new Dockerfile {
    from("openjdk:8-jre")
    add(artifact, artifactTargetPath)
//    env("IS_PRODUCTION", "true")
    entryPoint(
      "java",
      "-server",
      "-Xms150m",
      "-Xmx150m",
      "-XX:NewRatio=2",
      "-XX:+UseConcMarkSweepGC",
      "-XX:+CMSParallelRemarkEnabled",
      "-XX:+AlwaysPreTouch",
      "-XX:+HeapDumpOnOutOfMemoryError",
      "-jar",
      artifactTargetPath)
    expose(8000, 80)
  }
}

// Set a custom image name
imageNames in docker := {
  val imageName = ImageName("tbrown1979/avalon-game")
  Seq(imageName, imageName.copy(tag = Some(version.value))) //use the current version from version.sbt
}

lazy val publishDocker = ReleaseStep(action = st => {
  val extracted = Project.extract(st)
  val ref: ProjectRef = extracted.get(thisProjectRef)
  extracted.runAggregated(
    sbtdocker.DockerKeys.dockerBuildAndPush in sbtdocker.DockerPlugin.autoImport.docker in ref,
    st)
  st
})

import ReleaseTransformations._

releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,              // : ReleaseStep
  inquireVersions,                        // : ReleaseStep
  runClean,                               // : ReleaseStep
  runTest,                                // : ReleaseStep
  setReleaseVersion,                      // : ReleaseStep
  commitReleaseVersion,                   // : ReleaseStep, performs the initial git checks
  tagRelease,                             // : ReleaseStep
  publishDocker,
//  publishArtifacts,                       // We don't need to publish!
  setNextVersion,                         // : ReleaseStep
  commitNextVersion,                      // : ReleaseStep
  pushChanges                             // : ReleaseStep, also checks that an upstream branch is properly configured
)
