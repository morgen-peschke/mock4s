package peschke.mock4s

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.std.Console
import com.monovore.decline.Command
import com.monovore.decline.Opts
import peschke.mock4s.algebras.{JsonKeyExpander, JsonSourceResolver}
import peschke.mock4s.models.JsonSource
import peschke.mock4s.models.Settings

object ValidateSettings extends IOApp {
  private val command =
    Command(name = "ValidateSettings", header = "Settings validation utility")(Opts.argument[JsonSource]())

  override def run(args: List[String]): IO[ExitCode] = {
    val jsr = JsonSourceResolver.default[IO](JsonKeyExpander.default[IO])
    command
      .parse(args).fold(
        Console[IO].errorln(_).as(ExitCode.Error),
        source => jsr.resolve[Settings](source).as(ExitCode.Success)
      )
  }
}
