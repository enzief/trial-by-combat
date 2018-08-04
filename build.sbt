import com.round.Dependencies._

lazy val root = (project in file("."))
  .enablePlugins(com.round.ProjectPlugin)
  .enablePlugins(com.typesafe.sbt.packager.archetypes.JavaServerAppPackaging)
  .settings(
    name := "trial-by-combat",
    libraryDependencies ++= Seq(
      jgrapht,
      typesafeConfig,
      Cats.core,
      Cats.effect,
      Circe.core,
      Circe.literal,
      Circe.parser,
      Http4s.client,
      Http4s.circe,
      Http4s.dsl
    )
  )

addCommandAlias(
  "fmt",
  ";scalafmtSbt;scalafmt;test:scalafmt"
)

// while you're working, try putting "~wip" into your sbt console
// ...but be prepared to let IntelliJ force you to reload!
addCommandAlias(
  "wip",
  ";headerCreate;test:headerCreate;fmt;test:compile"
)
