val Http4sVersion = "0.20.9"
val CirceVersion = "0.11.1"
val catsParVersion = "0.2.1"
val enumeratumV = "1.5.13"
val Specs2Version = "4.1.0"
val LogbackVersion = "1.2.3"

//tests
val scalacheckMagnoliaV = "0.0.2"
val scalaTestV = "3.0.8"
val specs2V = "4.7.0"

lazy val root = (project in file("."))
  .settings(
    organization := "com.avalon",
    name := "avalon-game",
    version := "0.0.1-SNAPSHOT",
    scalaVersion := "2.12.8",
    libraryDependencies ++= Seq(
          "com.beachape"                %% "enumeratum"                   % enumeratumV,
"com.beachape" %% "enumeratum-circe" % enumeratumV,
      "org.http4s"      %% "http4s-blaze-server" % Http4sVersion,
      "org.http4s"      %% "http4s-blaze-client" % Http4sVersion,
      "org.http4s"      %% "http4s-circe"        % Http4sVersion,
      "org.http4s"      %% "http4s-dsl"          % Http4sVersion,
      "io.chrisdavenport" %% "cats-par" % catsParVersion,
      "io.circe"        %% "circe-generic"       % CirceVersion,
      "io.circe"        %% "circe-generic-extras" % CirceVersion,
      "io.circe" %% "circe-parser" % CirceVersion,
      "org.specs2"      %% "specs2-core"         % Specs2Version % "test",
      "ch.qos.logback"  %  "logback-classic"     % LogbackVersion,
       "com.beachape" %% "enumeratum-scalacheck" % enumeratumV,
          "com.mrdziuban"               %% "scalacheck-magnolia"          % scalacheckMagnoliaV,
    "org.scalatest"               %% "scalatest"                    % scalaTestV % Test,
    "org.specs2"                  %% "specs2-core"                  % specs2V % Test,
    "org.specs2"                  %% "specs2-matcher"               % specs2V % Test,
"org.specs2" %% "specs2-scalacheck" % specs2V % Test,
    ),
    addCompilerPlugin("org.typelevel" %% "kind-projector"     % "0.10.3"),
    addCompilerPlugin("com.olegpy"    %% "better-monadic-for" % "0.3.0")
  )

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-language:higherKinds",
  "-language:postfixOps",
  "-feature",
  "-Ypartial-unification",
  "-Xfatal-warnings",
)
