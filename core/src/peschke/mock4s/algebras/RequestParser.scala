package peschke.mock4s.algebras

import cats.effect.kernel.Concurrent
import cats.syntax.all._
import org.http4s.Request
import peschke.mock4s.models.ParsedRequest

trait RequestParser[F[_]] {
  def parse(request: Request[F]): F[ParsedRequest]
}
object RequestParser      {
  def apply[F[_]](implicit RP: RequestParser[F]): RP.type = RP

  def default[F[_]: Concurrent: BodyParser: StateManager]: RequestParser[F] =
    request =>
      (BodyParser[F].parse(request), StateManager[F].retrieve).tupled.map {
        case (body, state) =>
          ParsedRequest(
            ParsedRequest.Route(request.method, request.uri.path, request.uri.query, state),
            request.headers.headers,
            body
          )
      }
}
