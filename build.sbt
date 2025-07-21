import org.typelevel.sbt.gha.JavaSpec.Distribution
import xerial.sbt.Sonatype.sonatypeCentralHost
//import org.typelevel.sbt.gha._
//import org.typelevel.sbt.gha.GenerativePlugin.autoImport._

disablePlugins(TypelevelMimaPlugin) // we use our own versioning for now via gitflow-packager
//enablePlugins(TypelevelCiReleasePlugin, GenerativePlugin)

val scala3Version = "3.7.1"

inThisBuild(List(
  organization := "co.blocke",
  homepage := Some(url("https://github.com/gzoller/dynalens")),
  licenses := List("MIT" -> url("https://opensource.org/licenses/MIT")),
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/gzoller/dynalens"),
      "scm:git:git@github.com:gzoller/dynalens.git"
    )
  ),
  developers := List(
    Developer(
      "gzoller",
      "Greg Zoller",
      "gzoller@blocke.co",
      url("http://www.blocke.co")
    )
  )
))

ThisBuild / organization := "co.blocke"
ThisBuild / scalaVersion := "3.7.1"
ThisBuild / versionScheme := Some("semver-spec")
ThisBuild / githubWorkflowScalaVersions := Seq("3.7.1")
ThisBuild / githubWorkflowJavaVersions := Seq(org.typelevel.sbt.gha.JavaSpec(org.typelevel.sbt.gha.JavaSpec.Distribution.Temurin, "21"))
ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec(Distribution.Temurin, "21"))
ThisBuild / githubWorkflowOSes := Seq("ubuntu-latest", "windows-latest")
ThisBuild / sonatypeCredentialHost := sonatypeCentralHost
//ThisBuild / githubWorkflowGeneratedWorkflowYaml :=
//  githubWorkflowGeneratedWorkflowYaml.value.map { yaml =>
//    val permissionsBlock =
//      """permissions:
//        |  contents: write
//        |  security-events: write
//        |""".stripMargin
//
//    val lines = yaml.linesIterator.toList
//    val updated = lines match {
//      case head :: tail if head.trim.startsWith("name:") =>
//        (head :: permissionsBlock.linesIterator.toList ::: tail).mkString("\n")
//      case _ =>
//        yaml
//    }
//
//    updated
//  }

lazy val root = project
  .in(file("."))
  .settings(
    name := "dynalens",

    scalaVersion := scala3Version,
    javacOptions ++= Seq("--release", "21"),
    Compile / packageBin / mappings += {
      (baseDirectory.value / "plugin.properties") -> "plugin.properties"
    },
    doc := null,  // disable dottydoc for now
    Compile / doc / sources := Seq(),
    libraryDependencies ++= Seq(
      "co.blocke"      %% "scala-reflection"     % "2.0.16",
      "dev.zio"        %% "zio"                  % "2.1.19",
      "com.lihaoyi"    %% "fastparse"            % "3.1.1",
      "dev.zio"        %% "zio-test"             % "2.1.19" % Test,
      "dev.zio"        %% "zio-test-sbt"         % "2.1.19" % Test,
      "dev.zio"        %% "zio-test-magnolia"    % "2.1.19" % Test
    )
  )

ThisBuild / githubWorkflowPublishTargetBranches := Seq(
  RefPredicate.Equals(Ref.Branch("main")),
  RefPredicate.StartsWith(Ref.Tag("v"))
)

ThisBuild / githubWorkflowJobSetup := Seq(
  WorkflowStep.Run(
    name = Some("Ignore line ending differences in git"),
    cond = Some("contains(runner.os, 'windows')"),
    commands = List("bash -c 'git config --global core.autocrlf false'")
  ),
  WorkflowStep.Use(
    UseRef.Public("actions", "setup-java", "v4"),
    params = Map(
      "distribution" -> "temurin",
      "java-version" -> "21"
    )
  ),
  WorkflowStep.Use(
    UseRef.Public("actions", "checkout", "v4")
  ),
  WorkflowStep.Use(
    UseRef.Public("coursier", "setup-action", "v1")
  ),
  WorkflowStep.Run(
    name = Some("Install sbt"),
    commands = List(
      "cs install sbt",
      "echo \"$HOME/.local/share/coursier/bin\" >> $GITHUB_PATH",
      "sbt sbtVersion"
    )
  )
)

ThisBuild / githubWorkflowPublish := Seq(
  WorkflowStep.Sbt(
    List("ci-release"),
    env = Map(
      "PGP_PASSPHRASE" -> "${{ secrets.PGP_PASSPHRASE }}",
      "PGP_SECRET" -> "${{ secrets.PGP_SECRET }}",
      "SONATYPE_PASSWORD" -> "${{ secrets.SONATYPE_PASSWORD }}",
      "SONATYPE_USERNAME" -> "${{ secrets.SONATYPE_USERNAME }}",
      "CI_SNAPSHOT_RELEASE" -> "+publishSigned"
    )
  )
)