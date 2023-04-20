package peschke.mock4s

import cats.MonadThrow
import cats.effect.kernel.Async
import cats.effect.std.Console
import cats.syntax.all._
import ciris.{ConfigDecoder, ConfigError}
import com.comcast.ip4s
import com.comcast.ip4s.{Host, Ipv6Address, Port}
import com.monovore.decline.{Argument, Command, Opts}
import io.circe.parser
import org.http4s.Uri
import peschke.mock4s.algebras.JsonSourceResolver
import peschke.mock4s.models.{JsonSource, Settings}

import java.nio.file.{InvalidPathException, Path => NioPath, Paths => NioPaths}

final case class Config(host: Host, port: Port, settings: Settings, settingsRoot: Uri.Path)
object Config {
  trait Loader[F[_]] {
    def load: F[Config]
  }

  private val DefaultHost: Ipv6Address = ip4s.Ipv6Address.fromBigInt(BigInt(1))
  private val DefaultSettingsRoot: Uri.Path = Uri.Path.Root / "mock4s"

  def parse[F[_]: MonadThrow: Console : JsonSourceResolver](args: Seq[String]): Loader[F] = new Loader[F] {
    implicit val hostArg: Argument[Host] = Argument.from("host") { raw =>
      Host.fromString(raw).toValidNel(s"Unable to parse host from $raw")
    }

    implicit val portArg: Argument[Port] = Argument.from("port") { raw =>
      Port.fromString(raw).toValidNel(s"Unable to parse port from $raw")
    }

    implicit val uriPathArg: Argument[Uri.Path] = Argument.from("path") {
      Uri.Path.unsafeFromString(_).validNel
    }

    private val hostOpt =
      Opts
        .option[Host](long = "host", help = "The host to bind (defaults to localhost)")
        .orElse(DefaultHost.pure[Opts])

    private val portOpt = Opts.option[Port](long = "port", help = "The port to listen on")

    private val settingsJsonOpt =
      Opts.option[JsonSource](
        long = "settings",
        help = """The mock settings, as JSON, as a literal string or a path to a file or "-" to read from stdin"""
      )

    private val settingsRootOpt =
      Opts
        .option[Uri.Path](
          long = "settings-root",
          help = "Override the root for the settings endpoints, in the case of a conflict"
        )
        .orElse(DefaultSettingsRoot.pure[Opts])

    private def die: F[Config] = new IllegalArgumentException().raiseError[F, Config]

    override def load: F[Config] = {
      val command = Command(name = "mock4s", header = "Mock API server built on Http4s")(
        (hostOpt, portOpt, settingsJsonOpt, settingsRootOpt).tupled
      )
      command
        .parse(args, sys.env)
        .fold(
          help => Console[F].errorln(help) >> die,
          { case (host, port, settingsJson, settingsRoot) =>
            JsonSourceResolver[F]
              .resolve[Settings](settingsJson)
              .map(Config(host, port, _, settingsRoot))
          }
        )
    }
  }

  def default[F[_]: Async]: Loader[F] = new Loader[F] {
    private implicit val hostDecoder: ConfigDecoder[String, Host] =
      ConfigDecoder[String].mapOption("Host")(Host.fromString)

    private implicit val portDecoder: ConfigDecoder[String, Port] =
      ConfigDecoder[String].mapOption("Port")(Port.fromString)

    private implicit val settingsDecoder: ConfigDecoder[String, Settings] =
      ConfigDecoder[String].mapEither { (key, raw) =>
        parser.decode[Settings](raw).leftMap { error =>
          ConfigError.decode("Settings", key, raw).and(ConfigError(s"Invalid settings json: ${error.show}"))
        }
      }

    private implicit val nioPathDecoder: ConfigDecoder[String, NioPath] =
      ConfigDecoder[String].mapEither { (key, raw) =>
        Either.catchOnly[InvalidPathException](NioPaths.get(raw)).leftMap { error =>
          ConfigError.decode("Path", key, raw).and(ConfigError(s"Invalid path: ${error.getMessage}"))
        }
      }

    private implicit val uriPathDecoder: ConfigDecoder[String, Uri.Path] =
      ConfigDecoder[String].map(Uri.Path.unsafeFromString)

    override def load: F[Config] = {
      val host = ciris.env("MOCK_4S_HOST").as[Host].default(DefaultHost)
      val port = ciris.env("MOCK_4S_PORT").as[Port]
      val settingsJson = ciris.env("SETTINGS").as[Settings]
      val settingsFile = ciris.env("SETTINGS_FILE").as[NioPath].flatMap(ciris.file(_).as[Settings])
      val settingsEndpoint = ciris.env("SETTINGS_ENDPOINT").as[Uri.Path].default(DefaultSettingsRoot)
      (host, port, settingsJson.or(settingsFile), settingsEndpoint).mapN(Config.apply).load[F]
    }
  }
}
