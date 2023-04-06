package peschke.mock4s.models

import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax._
import peschke.mock4s.utils.Circe._

final case class Settings (mocks: List[MockDefinition])
object Settings {
  implicit val decoder: Decoder[Settings] = accumulatingDecoder { c =>
    c.downField("mocks").asAcc[List[MockDefinition]].map(Settings.apply)
  }
  implicit val encoder: Encoder[Settings] = Encoder.instance { settings =>
    Json.obj("mocks" := settings.mocks)
  }
}