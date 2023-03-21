package peschke.mock4s.models

import cats.syntax.all._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}
import org.http4s.Uri.Path
import org.http4s.{Method, Query}
import peschke.mock4s.models.MockDefinition.{Action, RouteDef}
import peschke.mock4s.predicates.Predicate
import peschke.mock4s.utils.Circe._

final case class MockDefinition(
                                 route: RouteDef,
                                 actions: List[Action]
                               )
object MockDefinition {
  object SimpleMethodPredicate extends Predicate.Simple[Method]
  object SimplePathPredicate extends Predicate.Simple[Path]
  object SimpleQueryPredicate extends Predicate.Simple[Query]

  final case class RouteDef(method: SimpleMethodPredicate.ADT, path: SimplePathPredicate.ADT)
  final case class Action(when: RequestPattern, respondWith: ResponseDef)

  implicit val routeDefDecoder: Decoder[RouteDef] = accumulatingDecoder { c =>
    (
      c.downField("method").asAcc[SimpleMethodPredicate.ADT],
      c.downField("path").asAcc[SimplePathPredicate.ADT]
    ).mapN(RouteDef.apply)
  }
  implicit val routeDefEncoder: Encoder[RouteDef] = Encoder.instance { rd =>
    Json.obj("method" := rd.method, "path" := rd.path)
  }

  implicit val actionDecoder: Decoder[Action] = accumulatingDecoder { c =>
    (
      c.downField("when").asAcc[RequestPattern],
      c.downField("respond-with").asAcc[ResponseDef]
    ).mapN(Action.apply)
  }
  implicit val actionEncoder: Encoder[Action] = Encoder.instance { a =>
    Json.obj(
      "when" := a.when,
      "respond-with" := a.respondWith
    )
  }

  implicit val decoder: Decoder[MockDefinition] = accumulatingDecoder { c =>
    (
      c.downField("route").asAcc[RouteDef],
      c.downField("actions").asAcc[List[Action]]
    ).mapN(MockDefinition.apply)
  }
  implicit val encoder: Encoder[MockDefinition] = Encoder.instance { mockDef =>
    Json.obj(
      "route" := mockDef.route,
      "actions" := mockDef.actions
    )
  }
}