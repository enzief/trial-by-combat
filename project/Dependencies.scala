package com.round

import sbt._

object Dependencies {

  object SbtPlugin {
    val kindProjector: ModuleID = "org.spire-math" %% "kind-projector" % "0.9.7"

    val scalafmt: ModuleID           = "com.geirsson"      % "sbt-scalafmt"        % "1.6.0-RC1"
    val partialUnification: ModuleID = "org.lyranthe.sbt"  % "partial-unification" % "1.1.0"
    val sbtRevolver: ModuleID        = "io.spray"          % "sbt-revolver"        % "0.9.1"
    val coursier: ModuleID           = "io.get-coursier"   % "sbt-coursier"        % "1.0.3"
    val sbtHeader: ModuleID          = "de.heikoseeberger" % "sbt-header"          % "5.0.0"
    val scalafix: ModuleID           = "ch.epfl.scala"     % "sbt-scalafix"        % "0.5.10"
    val buildInfo: ModuleID          = "com.eed3si9n"      % "sbt-buildinfo"       % "0.7.0"
    val packager: ModuleID           = "com.typesafe.sbt"  % "sbt-native-packager" % "1.3.4"
  }

  val typesafeConfig: ModuleID = "com.typesafe" % "config"       % "1.3.3"
  val jgrapht: ModuleID        = "org.jgrapht"  % "jgrapht-core" % "1.2.0"

  object Cats {
    val catsVersion: String   = "1.2.0"
    val effectVersion: String = "0.10.1"

    val core: ModuleID   = "org.typelevel" %% "cats-core"   % catsVersion
    val effect: ModuleID = "org.typelevel" %% "cats-effect" % effectVersion
  }

  object Circe {
    val version: String = "0.9.3"

    val core: ModuleID    = "io.circe" %% "circe-generic" % version
    val literal: ModuleID = "io.circe" %% "circe-literal" % version
    val parser: ModuleID  = "io.circe" %% "circe-parser"  % version
  }

  object Http4s {
    val version: String = "0.18.15"

    val blaze: ModuleID = "org.http4s" %% "http4s-blaze-server" % version
    val circe: ModuleID = "org.http4s" %% "http4s-circe"        % version
    val dsl: ModuleID   = "org.http4s" %% "http4s-dsl"          % version
  }
}
