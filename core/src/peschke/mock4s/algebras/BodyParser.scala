package peschke.mock4s.algebras

import cats.effect.kernel.Concurrent
import cats.syntax.all._
import org.http4s.Request
import peschke.mock4s.models.Body.Base64String
import peschke.mock4s.models.ParsedBody

trait BodyParser[F[_]] {
  def parse(request: Request[F]): F[ParsedBody]
}
object BodyParser      {
  def apply[F[_]](implicit BP: BodyParser[F]): BP.type = BP

  def default[F[_]: Concurrent]: BodyParser[F] = new BodyParser[F] {
    def decodeBytes(request: Request[F]): F[Either[ParsedBody, Vector[Byte]]] =
      request
        .body
        .compile
        .toVector
        .map { bytes =>
          if (bytes.isEmpty) ParsedBody.EmptyBody.asLeft
          else bytes.asRight[ParsedBody]
        }

    def decodeText(bytes: Vector[Byte]): F[Either[ParsedBody, (String, Base64String)]] =
      if (bytes.isEmpty) ParsedBody.EmptyBody.asLeft.pure[F].widen
      else
        fs2
          .Stream
          .emits[F, Byte](bytes)
          .through(fs2.text.utf8.decode)
          .compile
          .string
          .attempt
          .map { result =>
            val hexString = Base64String.of(bytes)
            result.bimap(
              e => ParsedBody.RawBody(Base64String.of(bytes), e.getMessage),
              _ -> hexString
            )
          }

    def decodeJson(text: (String, Base64String)): F[Either[ParsedBody, ParsedBody.JsonBody]] =
      io.circe.parser.parse(text._1)
        .bimap(
          pf => ParsedBody.TextBody(text._1, text._2, pf.show),
          json => ParsedBody.JsonBody(json, text._1, text._2)
        )
        .pure[F]
        .widen

    override def parse(request: Request[F]): F[ParsedBody] =
      decodeBytes(request)
        .flatMap(_.flatTraverse(decodeText))
        .flatMap(_.traverse(decodeJson))
        .map(_.map(_.merge).merge)
  }
}
