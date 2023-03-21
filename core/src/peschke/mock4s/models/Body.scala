package peschke.mock4s.models

import cats.Order
import cats.data.{NonEmptyList, Validated}
import cats.syntax.all._
import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import io.circe.syntax._
import peschke.mock4s.utils.Circe._

sealed abstract class Body
object Body {
  final case object Empty extends Body
  final case class TextBody(text: String) extends Body
  final case class JsonBody(json: Json) extends Body
  final case class Bytes(hexString: HexString) extends Body

  object HexString extends supertagged.NewType[BigInt] {
    def of(bytes: Array[Byte]): HexString = apply(BigInt(bytes))

    implicit val decoder: Decoder[Type] = accumulatingDecoder { c =>
      c.asAcc[String]
        .andThen { raw =>
          Validated
            .catchOnly[NumberFormatException](BigInt(raw, 16))
            .leftMap(_ => DecodingFailure("Expected hex digits (no leading '0x')", c.history).pure[NonEmptyList])
        }
        .map(apply(_))
    }
    implicit val encoder: Encoder[Type] = Encoder[String].contramap { hs =>
      raw(hs).toString(16)
    }
    implicit val order: Order[Type] = Order.by(raw)
  }
  type HexString = HexString.Type

  implicit val decoder: Decoder[Body] = anyOf[Body](
    fixed(true).as(Empty).at("empty").widen,
    accumulatingDecoder(_.asAcc[String].map(TextBody)).at("text").widen,
    accumulatingDecoder(_.asAcc[Json].map(JsonBody)).at("json").widen,
    accumulatingDecoder(_.asAcc[HexString].map(Bytes)).at("bytes").widen
  )

  implicit val encoder: Encoder[Body] = Encoder.instance {
    case Empty => Json.fromString("empty")
    case TextBody(value) => Json.obj("text" := value)
    case JsonBody(value) => Json.obj("json" -> value)
    case Bytes(hexString) => Json.obj("bytes" := hexString)
  }
}
