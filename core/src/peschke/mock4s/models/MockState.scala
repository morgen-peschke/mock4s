package peschke.mock4s.models

import cats.{Eq, Show}
import io.circe._

object MockState {
  object Key extends supertagged.NewType[String] {
    implicit val eq: Eq[Type] = Eq.by(raw)
    implicit val show: Show[Type] = Show.show(raw)
    implicit val encoder: Encoder[Type] = Encoder[String].contramap(raw)
    implicit val decoder: Decoder[Type] = Decoder[String].map(apply(_))
    implicit val keyDecoder: KeyDecoder[Type] = KeyDecoder[String].map(apply(_))
    implicit val keyEncoder: KeyEncoder[Type] = KeyEncoder[String].contramap(raw)
  }
  type Key = Key.Type

  object State extends supertagged.NewType[Map[Key, Json]] {
    implicit val eq: Eq[Type] = Eq.by(raw)
    implicit val encoder: Encoder[Type] = Encoder[Map[Key, Json]].contramap(raw)
    implicit val decoder: Decoder[Type] = Decoder[Map[Key, Json]].map(apply(_))

    val Empty: Type = apply(Map.empty[Key, Json])
  }
  type State = State.Type
}
