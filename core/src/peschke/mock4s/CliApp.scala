package peschke.mock4s

import cats.syntax.all._
import cats.effect.{ExitCode, IO, IOApp}
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory

object CliApp extends IOApp {

  implicit val logging: LoggerFactory[IO] = Slf4jFactory[IO]

  override def run(args: List[String]): IO[ExitCode] =
    Config.parse[IO](args).load.redeemWith(
      _ => ExitCode.Error.pure[IO],
      SetupServer.setup[IO](_).useForever.as(ExitCode.Success)
    )
}
