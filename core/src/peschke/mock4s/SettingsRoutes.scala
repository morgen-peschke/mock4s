package peschke.mock4s

import cats.Monad
import org.http4s._
import org.http4s.dsl.Http4sDsl
import org.http4s.circe.CirceEntityEncoder.circeEntityEncoder
import peschke.mock4s.models.Settings

object SettingsRoutes {
  def default[F[_] : Monad](endpointRoot: Uri.Path, settings: Settings): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F] {}
    import dsl._
    val RootPath = endpointRoot

    HttpRoutes.of[F] {
      case GET -> RootPath / "settings" => Ok(settings)
    }
  }
}
