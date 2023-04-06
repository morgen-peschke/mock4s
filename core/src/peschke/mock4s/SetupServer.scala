package peschke.mock4s

import cats.effect.kernel.{Async, Resource}
import fs2.io.net.Network
import org.http4s.HttpApp
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.Server
import org.http4s.server.middleware.Logger
import org.typelevel.log4cats.LoggerFactory
import peschke.mock4s.algebras.{BodyParser, RequestMatcher, RequestParser, ResponseRenderer}

object SetupServer {
  def setup[F[_]: Async : Network : LoggerFactory](config: Config): Resource[F, Server] = {
    implicit val bodyParser: BodyParser[F] = BodyParser.default[F]
    implicit val requestParser: RequestParser[F] = RequestParser.default[F]
    implicit val renderer: ResponseRenderer[F] = ResponseRenderer.default[F]
    implicit val matcher: RequestMatcher[F] = RequestMatcher.default[F](config.settings.mocks)

    val settingsRoutes = SettingsRoutes.default[F](config.settingsRoot, config.settings)
    val mockApp = MockApp.default[F]

    val app = Logger.httpApp[F](logHeaders = true, logBody = true)(HttpApp { request =>
      settingsRoutes.run(request).getOrElseF(mockApp.run(request))
    })

    Resource.eval(LoggerFactory[F].fromClass(classOf[Server])).flatMap { serverLogger =>
        EmberServerBuilder
          .default[F]
          .withHost(config.host)
          .withPort(config.port)
          .withLogger(serverLogger)
          .withHttpApp(app)
          .build
    }
  }
}
