package peschke.mock4s

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.all._
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory
import peschke.mock4s.algebras.JsonSourceResolver

object CliApp extends IOApp {

  implicit val logging: LoggerFactory[IO] = Slf4jFactory[IO]

  override def run(args: List[String]): IO[ExitCode] = {
    val logger = LoggerFactory.getLogger[IO]
    implicit val jsonSourceResolver: JsonSourceResolver[IO] = JsonSourceResolver.default[IO]
    Config
      .parse[IO](args).load.redeemWith(
        logger.error(_)("Unable to start up") >> ExitCode.Error.pure[IO],
        SetupServer.run[IO](_).as(ExitCode.Success)
      )
  }
}
