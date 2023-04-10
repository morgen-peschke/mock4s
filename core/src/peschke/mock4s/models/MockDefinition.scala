package peschke.mock4s.models

import cats.syntax.all._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}
import peschke.mock4s.models.MockDefinition.Action
import peschke.mock4s.predicates.{RequestPredicate, RoutePredicate}
import peschke.mock4s.utils.Circe._

final case class MockDefinition(name: String, route: RoutePredicate.Type, actions: List[Action])
object MockDefinition {
  final case class Action(name: String, when: RequestPredicate.Type, respondWith: ResponseDef)

  implicit val actionDecoder: Decoder[Action] = accumulatingDecoder { c =>
    (
      c.downField("name").asAcc[String],
      c.downField("when").asAcc[RequestPredicate.Type],
      c.downField("respond-with").asAcc[ResponseDef]
    ).mapN(Action.apply)
  }
  implicit val actionEncoder: Encoder[Action] = Encoder.instance { a =>
    Json.obj(
      "name"         := a.name,
      "when"         := a.when,
      "respond-with" := a.respondWith
    )
  }

  implicit val decoder: Decoder[MockDefinition] = accumulatingDecoder { c =>
    (
      c.downField("name").asAcc[String],
      c.downField("route").asAcc[RoutePredicate.Type],
      c.downField("actions").asAcc[List[Action]]
    ).mapN(MockDefinition.apply)
  }
  implicit val encoder: Encoder[MockDefinition] = Encoder.instance { mockDef =>
    Json.obj(
      "name"    := mockDef.name,
      "route"   := mockDef.route,
      "actions" := mockDef.actions
    )
  }
}
