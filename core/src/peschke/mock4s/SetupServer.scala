package peschke.mock4s

import cats.effect.kernel.Async
import cats.effect.kernel.Resource
import fs2.io.net.Network
import org.http4s.HttpApp
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.Server
import org.http4s.server.middleware.Logger
import org.typelevel.log4cats.LoggerFactory
import peschke.mock4s.algebras.BodyParser
import peschke.mock4s.algebras.MocksManager
import peschke.mock4s.algebras.RequestMatcher
import peschke.mock4s.algebras.RequestParser
import peschke.mock4s.algebras.ResponseRenderer
import peschke.mock4s.algebras.StateManager
import peschke.mock4s.algebras.StateUpdater

object SetupServer {
  def run[F[_]: Async: Network: LoggerFactory](config: Config): F[Nothing] =
    SetupServer.setup[F](config).useForever

  def setup[F[_]: Async: Network: LoggerFactory](config: Config): Resource[F, Server] =
    Resource.eval(MocksManager.initialize[F](config.settings.mocks)).flatMap { implicit mocksManager =>
      Resource.eval(StateManager.initialize[F](config.settings.initialState)).flatMap { implicit stateManager =>
        implicit val bodyParser: BodyParser[F] = BodyParser.default[F]
        implicit val requestParser: RequestParser[F] = RequestParser.default[F]
        implicit val renderer: ResponseRenderer[F] = ResponseRenderer.default[F]
        implicit val matcher: RequestMatcher[F] = RequestMatcher.default[F]
        implicit val stateUpdater: StateUpdater[F] = StateUpdater.default[F]

        val settingsRoutes = AdminRoutes.default[F](config.settingsRoot)
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
}
