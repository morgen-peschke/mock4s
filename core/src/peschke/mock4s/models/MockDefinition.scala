package peschke.mock4s.models

import cats.data.{Chain, NonEmptyChain}
import cats.{Eq, Order, Show}
import cats.syntax.all._
import io.circe.syntax._
import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import io.circe.CursorOp.DownN
import peschke.mock4s.models.MockDefinition.{Action, MockName}
import peschke.mock4s.predicates.{RequestPredicate, RoutePredicate}
import peschke.mock4s.utils.Circe._

final case class MockDefinition(name: MockName, route: RoutePredicate.Type, actions: Chain[Action])
object MockDefinition {
  object MockName extends supertagged.NewType[String] {
    val Empty: Type = apply("")
    implicit val eq: Eq[Type] = Eq.by(raw)
    implicit val show: Show[Type] = Show.show(raw)
    implicit val decoder: Decoder[Type] = Decoder[String].map(apply(_))
    implicit val encoder: Encoder[Type] = Encoder[String].contramap(raw)
    implicit val order: Order[Type] = Order.by(raw)
    def unapply(str: String): Some[Type] = Some(apply(str))
  }
  type MockName = MockName.Type

  object ActionName extends supertagged.NewType[String] {
    val Empty: Type = apply("")
    implicit val eq: Eq[Type] = Eq.by(raw)
    implicit val show: Show[Type] = Show.show(raw)
    implicit val decoder: Decoder[Type] = Decoder[String].map(apply(_))
    implicit val encoder: Encoder[Type] = Encoder[String].contramap(raw)
    implicit val order: Order[Type] = Order.by(raw)
    def unapply(str: String): Some[Type] = Some(apply(str))
  }
  type ActionName = ActionName.Type

  final case class Action(name: ActionName,
                          when: RequestPredicate.Type,
                          respondWith: ResponseDef,
                         )

  implicit val actionDecoder: Decoder[Action] = accumulatingDecoder { c =>
    (
      c.downField("name").asAcc[ActionName],
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

  implicit val actionEq: Eq[Action] = Eq.instance { (a,b) =>
    a.name === b.name && a.when === b.when && a.respondWith === b.respondWith
  }

  implicit val decoder: Decoder[MockDefinition] = accumulatingDecoder { c =>
    val actionsC = c.downField("actions")
    (
      c.downField("name").asAcc[MockName],
      c.downField("route").asAcc[RoutePredicate.Type],
      actionsC
        .asAcc[Chain[Action]]
        .map {
          _.mapWithIndex { (action, index) =>
            if (action.name =!= ActionName.Empty) action -> index
            else action.copy(name = ActionName(s"action $index")) -> index
          }
        }
        .andThen { actions =>
          val errors: Chain[DecodingFailure] =
            Chain.fromSeq(actions.groupBy(_._1.name).toSeq)
              .flatMap {
                case (_, Chain(_)) => Chain.empty[DecodingFailure]
                case (name, duplicates) =>
                  duplicates.toChain.map { case (_, i) =>
                    DecodingFailure(show"Duplicate action name: $name", DownN(i) :: actionsC.history)
                  }
              }

          NonEmptyChain
            .fromChain(errors)
            .fold(actions.map(_._1).validNel[DecodingFailure])(_.toNonEmptyList.invalid)
        }
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
