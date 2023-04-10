package peschke.mock4s.algebras

import cats.Applicative
import org.http4s.circe.CirceEntityEncoder.circeEntityEncoder
import org.http4s.{Headers, Response}
import peschke.mock4s.models.{Body, ResponseDef}

trait ResponseRenderer[F[_]] {
  def render(responseDef: ResponseDef): F[Response[F]]
}
object ResponseRenderer      {
  def apply[F[_]](implicit RR: ResponseRenderer[F]): RR.type = RR

  def default[F[_]: Applicative]: ResponseRenderer[F] =
    responseDef => {
      val base = Response[F](
        status = responseDef.status,
        httpVersion = responseDef.httpVersion,
        headers = Headers(responseDef.headers)
      )
      Applicative[F].pure(responseDef.body match {
        case Body.Empty          => base.withEmptyBody
        case Body.TextBody(text) => base.withEntity(text)
        case Body.JsonBody(json) => base.withEntity(json)
        case Body.Bytes(bytes)   => base.withEntity(bytes)
      })
    }
}
