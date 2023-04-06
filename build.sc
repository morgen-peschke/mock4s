import $ivy.`com.goyeau::mill-scalafix::0.2.10`
import com.goyeau.mill.scalafix.ScalafixModule
import mill._, scalalib._, scalafmt._
import mill.scalalib.publish._

val Scala13 = "2.13.8"

val Decline = ivy"com.monovore::decline:2.3.0"
val SuperTagged = ivy"org.rudogma::supertagged:2.0-RC2"
val Enumeratum = ivy"com.beachape::enumeratum:1.7.0"
val Circe = Set(
  ivy"io.circe::circe-core:0.14.1",
  ivy"io.circe::circe-parser:0.14.1"
)
val Ciris = ivy"is.cir::ciris:3.1.0"
val Http4s = Set(
  ivy"org.http4s::http4s-ember-client:1.0.0-M39",
  ivy"org.http4s::http4s-ember-server:1.0.0-M39",
  ivy"org.http4s::http4s-dsl:1.0.0-M39",
  ivy"org.http4s::http4s-circe:1.0.0-M39"
)
val Log4Cats = ivy"org.typelevel::log4cats-slf4j:2.5.0"
val MUnit = Set(
  ivy"org.scalameta::munit:0.7.29",
  ivy"org.scalacheck::scalacheck:1.17.0",
  ivy"org.scalameta::munit-scalacheck:0.7.29"
)
val SourceCode = ivy"com.lihaoyi::sourcecode:0.3.0"

trait StyleModule extends ScalafmtModule with ScalafixModule {
  override def scalafixIvyDeps = super.scalafixIvyDeps() ++ Agg(
    ivy"com.github.liancheng::organize-imports:0.6.0",
    ivy"org.typelevel::typelevel-scalafix:0.1.5"
  )

  override def scalacOptions =
    super.scalacOptions() ++ Seq(
      "-encoding",
      "UTF-8",
      "-deprecation",
      "-unchecked",
      "-feature",
      "-Ywarn-unused",
      "-Ywarn-dead-code",
      "-Ywarn-value-discard",
      "-Xfatal-warnings",
      "-language:higherKinds"
    )

  override def scalaDocOptions = super.scalaDocOptions() ++ Seq("-no-link-warnings")

  override def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(
    ivy"com.olegpy::better-monadic-for:0.3.1",
    ivy"org.typelevel:::kind-projector:0.13.2"
  )
}

trait CommonModule
    extends ScalaModule
    with StyleModule
    with PublishModule {

  def publishVersion: T[String] = "0.1.0"

  override def pomSettings: T[PomSettings] = PomSettings(
    description = "Mock4s - mocked API server built on Http4s",
    organization = "com.github.morgen-peschke",
    url = "https://github.com/morgen-peschke/mock4s",
    licenses = Seq(License.MIT),
    versionControl = VersionControl.github("morgen-peschke", "mock4s"),
    developers = Seq(
      Developer(
        "morgen-peschke",
        "Morgen Peschke",
        "https://github.com/morgen-peschke"
      )
    )
  )
}

object core extends CommonModule {
  override def scalaVersion: T[String] = Scala13

  override def ivyDeps =
    Agg(SourceCode, SuperTagged, Enumeratum, Ciris, Log4Cats, Decline) ++
      Agg.from(Http4s ++ Circe)

  override def runIvyDeps = Agg(ivy"ch.qos.logback:logback-classic:1.2.10")

  object test extends Tests with TestModule.Munit with StyleModule {
    override def ivyDeps: T[Agg[Dep]] = super.ivyDeps() ++ Agg.from(MUnit)
  }
}
