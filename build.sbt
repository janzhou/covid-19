lazy val covid_19 = project.in(file(".")).
  settings(
    name := "covid_19",
    scalaVersion := "2.12.10",
    scalaJSUseMainModuleInitializer := true,
    mainClass in Compile := Some("covid_19.GhPages"),
    libraryDependencies ++= Seq (
      "com.github.japgolly.scalajs-react" %%% "core" % "1.6.0",
      "com.github.japgolly.scalajs-react" %%% "extra" % "1.6.0",
      "org.scala-js" %%% "scalajs-dom" % "1.0.0",
    ),
    dependencyOverrides += "org.webjars.npm" % "js-tokens" % "3.0.2",
    npmDependencies in Compile ++= Seq(
      "react" -> "16.7.0",
      "react-dom" -> "16.7.0"
    )
  ).
  enablePlugins(ScalaJSPlugin, ScalaJSBundlerPlugin)