val scala3Version = "3.7.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "co/blocke/dynalens",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "co.blocke"      %% "scala-reflection"     % "2.0.16",
      "dev.zio"        %% "zio"                  % "2.1.19",
      "com.lihaoyi"    %% "fastparse"            % "3.1.1",
      "dev.zio"        %% "zio-test"             % "2.1.19" % Test,
      "dev.zio"        %% "zio-test-sbt"         % "2.1.19" % Test,
      "dev.zio"        %% "zio-test-magnolia"    % "2.1.19" % Test
    )
  )
