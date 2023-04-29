package peschke.mock4s.algebras

import cats.ApplicativeThrow
import cats.effect.Async
import cats.syntax.all._
import de.marhali.json5.Json5
import de.marhali.json5.exception.Json5Exception
import fs2.io.file.Files
import fs2.text
import io.circe.{Decoder, Json}
import io.circe.syntax._
import peschke.mock4s.models.JsonSource
import peschke.mock4s.utils.Circe._
import peschke.mock4s.utils.Json5Codecs._

trait JsonSourceResolver[F[_]] {
  def resolve[A: Decoder](jsonSource: JsonSource): F[A]
}
object JsonSourceResolver      {
  def apply[F[_]](implicit JSR: JsonSourceResolver[F]): JSR.type = JSR

  def default[F[_]: Async: Files]: JsonSourceResolver[F] = new JsonSourceResolver[F] {
    private val json5 = Json5.builder(_.quoteSingle().trailingComma().build())

    def parse(string: String): F[Json] =
      ApplicativeThrow[F]
        .catchOnly[Json5Exception](json5.parse(string))
        .redeemWith(
          e => new IllegalArgumentException(s"Malformed JSON5: ${e.getMessage}").raiseError[F, Json],
          _.asJson.pure[F]
        )

    def loadString(jsonSource: JsonSource): F[String] =
      jsonSource match {
        case JsonSource.LiteralString(raw) => raw.pure[F]
        case JsonSource.ReadStdIn          =>
          fs2.io.stdin[F](1024).through(text.utf8.decode).compile.string
        case JsonSource.LoadFile(path)     =>
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
