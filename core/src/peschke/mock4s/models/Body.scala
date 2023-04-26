package peschke.mock4s.models

import cats.{Eq, Order}
import cats.data.{NonEmptyList, Validated}
import cats.syntax.all._
import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import io.circe.syntax._
import peschke.mock4s.utils.Circe._

import java.nio.charset.Charset
import java.util.Base64

sealed abstract class Body
object Body {
  final case object Empty                      extends Body
  final case class TextBody(text: String)      extends Body
  final case class JsonBody(json: Json)        extends Body
  final case class Bytes(hexString: Base64String) extends Body

  object Base64String extends supertagged.NewType[String] {
    private val base64encoder: Base64.Encoder = Base64.getEncoder
    private val base64decoder: Base64.Decoder = Base64.getDecoder
    private val utf8 = Charset.forName("UTF-8")

    def of(bytes: Vector[Byte]): Base64String =
      apply(new String(base64encoder.encode(bytes.toArray), utf8))

    implicit val decoder: Decoder[Type] = accumulatingDecoder { c =>
      c.asAcc[String]
        .andThen { raw =>
          Validated
            .catchOnly[IllegalArgumentException](base64decoder.decode(raw.getBytes(utf8)))
            .bimap(
              _ => DecodingFailure("Expected base64 encoded bytes", c.history).pure[NonEmptyList],
              _ => apply(raw)
            )
        }
    }
    implicit val encoder: Encoder[Type] = Encoder[String].contramap(raw)
    implicit val order: Order[Type] = Order.by(raw)

    implicit final class Ops(private val t: Type) extends AnyVal {
      def bytes: Array[Byte] = base64decoder.decode(raw(t))
    }
  }
  type Base64String = Base64String.Type

  implicit val decoder: Decoder[Body] = anyOf[Body](
    fixed("empty").as(Empty).widen,
    accumulatingDecoder(_.asAcc[String].map(TextBody)).at("text").widen,
    accumulatingDecoder(_.asAcc[Json].map(JsonBody)).at("json").widen,
    accumulatingDecoder(_.asAcc[Base64String].map(Bytes)).at("bytes").widen
  )

  implicit val encoder: Encoder[Body] = Encoder.instance {
    case Empty            => Json.fromString("empty")
    case TextBody(value)  => Json.obj("text" := value)
    case JsonBody(value)  => Json.obj("json" -> value)
    case Bytes(hexString) => Json.obj("bytes" := hexString)
  }

  implicit val eq: Eq[Body] = Eq.instance {
    case (Empty, Empty) => true
    case (TextBody(a), TextBody(b)) => a === b
    case (JsonBody(a), JsonBody(b)) => a === b
    case (Bytes(a), Bytes(b)) => a === b
    case _ => false
  }
}
