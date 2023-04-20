package peschke.mock4s

import cats.effect.std.Console
import cats.effect.{ExitCode, IO, IOApp}
import com.monovore.decline.{Command, Opts}
import peschke.mock4s.algebras.JsonSourceResolver
import peschke.mock4s.models.{JsonSource, Settings}

object ValidateSettings extends IOApp {
  private val command =
    Command(name = "ValidateSettings", header = "Settings validation utility")(Opts.argument[JsonSource]())

  override def run(args: List[String]): IO[ExitCode] = {
    val jsr = JsonSourceResolver.default[IO]
    command.parse(args).fold(
      Console[IO].errorln(_).as(ExitCode.Error),
      source => jsr.resolve[Settings](source).as(ExitCode.Success)
    )
  }
}
