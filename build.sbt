import BuildHelper._

inThisBuild(
  List(
    organization  := "dev.zio",
    homepage      := Some(url("https://zio.dev/caliban-deriving/")),
    licenses      := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers    := List(
      Developer(
        "jdegoes",
        "John De Goes",
        "john@degoes.net",
        url("http://degoes.net")
      ),
      Developer(
        "vigoo",
        "Daniel Vigovszky",
        "daniel.vigovszky@gmail.com",
        url("https://github.com/vigoo")
      )
    ),
    pgpPassphrase := sys.env.get("PGP_PASSWORD").map(_.toArray),
    pgpPublicRing := file("/tmp/public.asc"),
    pgpSecretRing := file("/tmp/secret.asc")
  )
)

addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias("fix", "; all compile:scalafix test:scalafix; all scalafmtSbt scalafmtAll")
addCommandAlias("check", "; scalafmtSbtCheck; scalafmtCheckAll; compile:scalafix --check; test:scalafix --check")

addCommandAlias(
  "testJVM",
  ";calibanDeriving/test"
)

val zioVersion     = "1.0.18"
val calibanVersion = "1.2.4"

lazy val root = project
  .in(file("."))
  .settings(
    publish / skip := true
  )
  .aggregate(
    calibanDeriving
  )

lazy val calibanDeriving = project
  .in(file("caliban-deriving"))
  .settings(stdSettings("caliban-deriving"))
  .settings(dottySettings)
  .settings(buildInfoSettings("caliban.deriving"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio"               %% "zio"          % zioVersion,
      "com.github.ghostdogpr" %% "caliban"      % calibanVersion,
      "dev.zio"               %% "zio-test"     % zioVersion % Test,
      "dev.zio"               %% "zio-test-sbt" % zioVersion % Test
    )
  )
  .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))
  .enablePlugins(BuildInfoPlugin)

lazy val docs = project
  .in(file("caliban-deriving-docs"))
  .settings(stdSettings("caliban-deriving"))
  .settings(
    moduleName                                 := "caliban-deriving-docs",
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Xfatal-warnings",
    projectName                                := "Caliban Deriving",
    mainModuleName                             := (calibanDeriving / moduleName).value,
    projectStage                               := ProjectStage.Development,
    ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(calibanDeriving),
    docsPublishBranch                          := "master"
  )
  .dependsOn(calibanDeriving)
  .enablePlugins(WebsitePlugin)
