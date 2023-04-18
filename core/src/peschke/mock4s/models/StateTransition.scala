package peschke.mock4s.models

import cats.Eq
import cats.syntax.all._
import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax._
import peschke.mock4s.utils.Circe._

sealed trait StateTransition
object StateTransition {
  final case class Clear(key: MockState.Key) extends StateTransition
  final case class Set(key: MockState.Key, value: Json) extends StateTransition

  implicit val decoder: Decoder[StateTransition] = anyOf[StateTransition](
    accumulatingDecoder(_.downField("clear").downField("key").asAcc[MockState.Key].map(Clear)),
    accumulatingDecoder { c =>
      (
        c.downField("key").asAcc[MockState.Key],
        c.downField("value").asAcc[Json]
      ).mapN(Set)
    }.at("set").widen
  )

  implicit val encoder: Encoder[StateTransition] = Encoder.instance {
    case Clear(key) => Json.obj("clear" := Json.obj("key" := key))
    case Set(key, value) => Json.obj("set" := Json.obj("key" := key, "value" := value))
  }

  implicit val eq: Eq[StateTransition] = Eq.instance {
    case (Clear(ka), Clear(kb)) => ka === kb
    case (Set(ka, va), Set(kb, vb)) => ka === kb && va === vb
    case _ => false
  }
}
