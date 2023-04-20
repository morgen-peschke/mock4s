package peschke.mock4s.models

import cats.Eq
import cats.data.Chain
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}
import peschke.mock4s.utils.Circe._

sealed trait StateTransition
object StateTransition {
  final case class Clear(keys: Chain[MockState.Key]) extends StateTransition
  final case class Set(entries: Chain[(MockState.Key, Json)]) extends StateTransition

  implicit val decoder: Decoder[StateTransition] = anyOf[StateTransition](
    accumulatingDecoder(_.downField("clear").asAcc[Chain[MockState.Key]].map(Clear)),
    accumulatingDecoder(_.downField("set").asAcc[Map[MockState.Key, Json]].map { map =>
      Set(Chain.fromSeq(map.toSeq))
    })
  )

  implicit val encoder: Encoder[StateTransition] = Encoder.instance {
    case Clear(keys) => Json.obj("clear" := keys)
    case Set(entries) => Json.obj("set" := entries.toVector.toMap.asJsonObject)
  }

  implicit val eq: Eq[StateTransition] = Eq.instance {
    case (Clear(ka), Clear(kb)) => ka === kb
    case (Set(ka), Set(kb)) => ka === kb
    case _ => false
  }
}
