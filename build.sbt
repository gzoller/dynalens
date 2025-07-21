import org.typelevel.sbt.gha.GenerativePlugin.autoImport.*
import org.typelevel.sbt.gha.{JavaSpec, Ref, RefPredicate}
import org.typelevel.sbt.gha.JavaSpec.Distribution
import org.typelevel.sbt.gha.Permissions

enablePlugins(TypelevelPlugin, TypelevelSonatypePlugin, TypelevelCiReleasePlugin, TypelevelCiSigningPlugin)

val scala3Version = "3.7.1"

inThisBuild(
  List(
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
  )
)

//ThisBuild / version := "1.0.0-SNAPSHOT" // <-- Comment this line out for real releases!

ThisBuild / organization := "co.blocke"
ThisBuild / scalaVersion := scala3Version
ThisBuild / versionScheme := Some("early-semver")

// GitHub Actions setup
ThisBuild / githubWorkflowScalaVersions := Seq(scala3Version)
ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec(Distribution.Temurin, "21"))
ThisBuild / githubWorkflowOSes := Seq("ubuntu-latest")
ThisBuild / githubWorkflowPermissions := Some(Permissions.WriteAll)
ThisBuild / githubWorkflowPublishTargetBranches := Seq(
  RefPredicate.Equals(Ref.Branch("main"))
)
ThisBuild / mimaFailOnNoPrevious := false
ThisBuild / tlBaseVersion := "1.0"

// Only needed if you use publishing with TypelevelCiReleasePlugin
ThisBuild / githubWorkflowPublishTargetBranches := Seq(
  RefPredicate.Equals(Ref.Branch("main")),
  RefPredicate.StartsWith(Ref.Tag("v"))
)

lazy val root = project
  .in(file("."))
  .settings(
    name := "dynalens",
    Compile / packageBin / mappings += {
      (baseDirectory.value / "plugin.properties") -> "plugin.properties"
    },
    headerLicense := Some(HeaderLicense.MIT("2025", "Greg Zoller")),
    doc := null,
    Compile / doc / sources := Nil,
    libraryDependencies ++= Seq(
      "co.blocke" %% "scala-reflection" % "2.0.16",
      "dev.zio" %% "zio" % "2.1.19",
      "com.lihaoyi" %% "fastparse" % "3.1.1",
      "dev.zio" %% "zio-test" % "2.1.19" % Test,
      "dev.zio" %% "zio-test-sbt" % "2.1.19" % Test,
      "dev.zio" %% "zio-test-magnolia" % "2.1.19" % Test
    )
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
  WorkflowStep.Sbt(List("ci-release"),
    env = Map(
      "PGP_PASSPHRASE"      -> "${{ secrets.PGP_PASSPHRASE }}",
      "PGP_SECRET"          -> "${{ secrets.PGP_SECRET }}",
      "SONATYPE_USERNAME"   -> "${{ secrets.SONATYPE_USERNAME }}",
      "SONATYPE_PASSWORD"   -> "${{ secrets.SONATYPE_PASSWORD }}"
    )
  )
)
