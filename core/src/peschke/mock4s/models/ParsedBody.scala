package peschke.mock4s.models

import io.circe.Encoder
import io.circe.Json
import io.circe.syntax._
import peschke.mock4s.models.Body.Base64String

sealed trait ParsedBody
object ParsedBody {
  case object EmptyBody                                                         extends ParsedBody
  final case class JsonBody(json: Json, text: String, hex: Base64String)        extends ParsedBody
  final case class TextBody(text: String, hex: Base64String, jsonError: String) extends ParsedBody
  final case class RawBody(raw: Base64String, textError: String)                extends ParsedBody
  final case class CouldNotDecode(error: String)                                extends ParsedBody

  implicit val encoder: Encoder[ParsedBody] = Encoder.instance {
    case EmptyBody                 => "empty".asJson
    case JsonBody(json, text, hex) =>
      Json.obj(
        "json" -> json,
        "text" := text,
        "hex"  := hex
      )

    case TextBody(text, hex, jsonError) =>
      Json.obj(
        "text"       := text,
        "hex"        := hex,
        "json:error" := jsonError
      )

    case RawBody(hex, textError) =>
      Json.obj(
        "hex"        := hex,
        "text:error" := textError
      )

    case CouldNotDecode(error) => Json.obj("hex:error" := error)
  }
}
