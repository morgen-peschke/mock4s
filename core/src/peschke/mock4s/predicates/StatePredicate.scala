package peschke.mock4s.predicates

import cats.Eq
import cats.syntax.all._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}
import peschke.mock4s.models.MockState
import peschke.mock4s.predicates.Predicate.{Fixed, UsingCombinators}
import peschke.mock4s.utils.Circe._

object StatePredicate extends PredicateWrapper[MockState.State] {
  sealed trait Tests extends Predicate[MockState.State] {
    def upcast: Tests = this
  }
  object Tests {
    final case class IsCleared(key: MockState.Key) extends Tests {
      override def test(a: MockState.State): Boolean = !MockState.State.raw(a).contains(key)
    }
    final case class IsSet(key: MockState.Key, value: JsonPredicate.Type) extends Tests {
      override def test(a: MockState.State): Boolean =
        MockState.State.raw(a).get(key).exists(value.test)
    }

    implicit val decoder: Decoder[Tests] = anyOf[Tests](
      accumulatingDecoder(_.downField("cleared").asAcc[MockState.Key].map(IsCleared)),
      accumulatingDecoder { c =>
        (
          c.downField("key").asAcc[MockState.Key],
          c.downField("value").asAcc[JsonPredicate.Type]
        ).mapN(IsSet)
      }.widen
    )

    implicit val encoder: Encoder[Tests] = Encoder.instance {
      case IsCleared(key) => Json.obj("cleared" := key)
      case IsSet(key, value) => Json.obj("key" := key, "value" := value)
    }

    implicit val eq: Eq[Tests] = Eq.instance {
      case (IsCleared(ka), IsCleared(kb)) => ka === kb
      case (IsSet(ka, va), IsSet(kb, vb)) => ka === kb && va === vb
      case _ => false
    }
  }

  type Base = Tests |+| Fixed[MockState.State]

  override implicit val baseDecoder: Decoder[Base] = GeneratedDecoder[Base].decoder
  override implicit val baseEncoder: Encoder[Base] = GeneratedEncoder[Base].encoder

  val always: Type =
    Fixed.Always[MockState.State]()
      .fixed
      .rhs[Tests]
      .lhs[UsingCombinators[MockState.State, Base]]
      .wrapped

  val never: Type =
    Fixed.Never[MockState.State]()
      .fixed
      .rhs[Tests]
      .lhs[UsingCombinators[MockState.State, Base]]
      .wrapped

  def isCleared(key: MockState.Key): Type =
    Tests.IsCleared(key)
      .upcast
      .lhs[Fixed[MockState.State]]
      .lhs[UsingCombinators[MockState.State, Base]]
      .wrapped

  def isSet(key: MockState.Key, value: JsonPredicate.Type): Type =
    Tests.IsSet(key, value)
      .upcast
      .lhs[Fixed[MockState.State]]
      .lhs[UsingCombinators[MockState.State, Base]]
      .wrapped
}
