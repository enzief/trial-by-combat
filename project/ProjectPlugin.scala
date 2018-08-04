package com.round

import de.heikoseeberger.sbtheader.FileType
import de.heikoseeberger.sbtheader.HeaderPlugin, HeaderPlugin.autoImport._
import scalafix.sbt.ScalafixPlugin, ScalafixPlugin.autoImport._
import sbt._
import sbt.Keys._
import sbtbuildinfo.BuildInfoPlugin, BuildInfoPlugin.autoImport._

import scala.sys.process._
import scala.util._

/**
  * Common project settings.
  */
object ProjectPlugin extends AutoPlugin {

  override def requires: Plugins =
    BuildInfoPlugin && HeaderPlugin && ScalafixPlugin
  override def trigger = allRequirements

  override val buildSettings: Seq[Def.Setting[_]] = Seq(
    organization              := "com.round",
    version                   := "0.0.1-SNAPSHOT",
    scalaVersion              := "2.12.6",
    scalafixSemanticdbVersion := "4.0.0-M7",
    scalafixConfig            := Some(file("project/scalafix.conf")),
  )

  override val projectSettings: Seq[Def.Setting[_]] =
    scalafixSettings ++
      buildInfoSettings ++
      headerSettings ++
      Seq(
        // BLOCKED: https://github.com/coursier/coursier/issues/349
        conflictManager            := ConflictManager.default,
        publishArtifact in makePom := false,
        publish                    := {},
        publishLocal               := {},
        publishArtifact            := false,
        autoAPIMappings in Global  := true,
        addCompilerPlugin(Dependencies.SbtPlugin.kindProjector),
        scalacOptions ++= commonScalacOptions ++ scalacOptionsFor212
      )

  private lazy val buildInfoSettings: Seq[Def.Setting[_]] = Seq(
    buildInfoKeys := Seq[BuildInfoKey](
      name in ThisBuild,
      version,
      scalaVersion,
      sbtVersion,
      BuildInfoKey.action("lastCommitSha")(gitCommitHash)
    ),
    buildInfoPackage :=
      s"${organization.value}.build",
    buildInfoOptions += BuildInfoOption.BuildTime,
    buildInfoOptions += BuildInfoOption.ToJson
  )

  private def gitCommitHash =
    Try("git rev-parse --verify HEAD".!!.trim).getOrElse("n/a")

  private lazy val headerSettings: Seq[Def.Setting[_]] = Seq(
    startYear := Some(2018),
    licenses  := Nil,
    headerLicense := Some(
      HeaderLicense.Custom(
        "Copyright (c) 2018 Round Inc.. All rights reserved."
      )
    ),
    headerMappings := headerMappings.value ++ Map(
      FileType("sbt") -> HeaderCommentStyle.cppStyleLineComment,
      HeaderFileType.java -> HeaderCommentStyle.cppStyleLineComment,
      HeaderFileType.scala -> HeaderCommentStyle.cppStyleLineComment
    )
  )

  // See https://docs.scala-lang.org/overviews/compiler-options/index.html
  private lazy val scalacOptionsFor212 = Seq(
    "-opt:l:inline",
    "-opt-inline-from:com.round.**:cats.**:org.http4s.**",
    "-opt:unreachable-code",
    "-opt:simplify-jumps",
    "-opt:compact-locals",
    "-opt:copy-propagation",
    "-opt:redundant-casts",
    "-opt:box-unbox",
    "-opt:nullness-tracking",
    "-opt:closure-invocations",
    "-Xlint:constant",
    "-Ywarn-extra-implicit",
    "-Ywarn-unused:implicits",
    "-Ywarn-unused:imports",
    "-Ywarn-unused:locals",
    "-Ywarn-unused:params",
    "-Ywarn-unused:patvars",
    "-Ywarn-unused:privates"
  )

  private lazy val commonScalacOptions = Seq(
    "-deprecation",
    "-encoding",
    "utf-8",
    "-explaintypes",
    "-feature",
    "-language:existentials",
    "-language:experimental.macros",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-language:postfixOps",
    "-unchecked",
    "-Xfuture",
    "-Xlint:adapted-args",
    "-Xlint:by-name-right-associative",
    "-Xlint:delayedinit-select",
    "-Xlint:doc-detached",
    "-Xlint:inaccessible",
    "-Xlint:infer-any",
    "-Xlint:missing-interpolator",
    "-Xlint:nullary-override",
    "-Xlint:nullary-unit",
    "-Xlint:option-implicit",
    "-Xlint:package-object-classes",
    "-Xlint:poly-implicit-overload",
    "-Xlint:private-shadow",
    "-Xlint:stars-align",
    "-Xlint:type-parameter-shadow",
    "-Xlint:unsound-match",
    "-Yno-adapted-args",
    "-Ypartial-unification",
    "-Ywarn-dead-code",
    "-Ywarn-inaccessible",
    "-Ywarn-infer-any",
    "-Ywarn-nullary-override",
    "-Ywarn-nullary-unit",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard"
  )
}
