package peschke.mock4s.algebras

import cats.effect.Async
import cats.syntax.all._
import fs2.io.file.Files
import fs2.text
import io.circe.{Decoder, Json, parser}
import peschke.mock4s.models.JsonSource
import peschke.mock4s.utils.Circe._

trait JsonSourceResolver[F[_]] {
  def resolve[A: Decoder](jsonSource: JsonSource): F[A]
}
object JsonSourceResolver {
  def apply[F[_]](implicit JSR: JsonSourceResolver[F]): JSR.type = JSR

  def default[F[_]: Async: Files]: JsonSourceResolver[F] = new JsonSourceResolver[F] {

    def parse(string: String): F[Json] =
      parser.parse(string)
        .leftMap(e => new IllegalArgumentException(s"Malformed JSON: ${e.getMessage}"))
        .liftTo[F]

    def loadString(jsonSource: JsonSource): F[String] =
      jsonSource match {
        case JsonSource.LiteralString(raw) => raw.pure[F]
        case JsonSource.ReadStdIn =>
          fs2.io.stdin[F](1024).through(text.utf8.decode).compile.string
        case JsonSource.LoadFile(path) =>
          Files[F].readAll(path).through(text.utf8.decode).compile.string
      }

    def decode[A: Decoder](json: Json): F[A] =
      json
        .hcursor
        .asAcc[A]
        .leftMap { decodeErrors =>
          new IllegalArgumentException(
            decodeErrors.mkString_("Invalid settings JSON:\n", "\n", "\n")
          )
        }
        .liftTo[F]

    override def resolve[A: Decoder](jsonSource: JsonSource): F[A] =
      loadString(jsonSource)
        .flatMap(parse)
        .flatMap(decode[A])
  }
}
