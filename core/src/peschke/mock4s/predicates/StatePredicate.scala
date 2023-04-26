package peschke.mock4s.predicates

import cats.Eq
import cats.syntax.all._
import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax._
import peschke.mock4s.algebras.PredicateChecker
import peschke.mock4s.algebras.PredicateChecker.syntax._
import peschke.mock4s.models.MockState
import peschke.mock4s.models.|+|
import peschke.mock4s.models.|+|.syntax._
import peschke.mock4s.utils.Circe._

sealed trait StateTests {
  def upcast: StateTests = this
}

object StateTests {
  final case class IsCleared(key: MockState.Key) extends StateTests

  final case class IsSet(key: MockState.Key, value: JsonPredicate.Type) extends StateTests

  implicit val decoder: Decoder[StateTests] = anyOf[StateTests](
    accumulatingDecoder(_.downField("cleared").asAcc[MockState.Key].map(IsCleared)),
    Decoder[JsonObjectTuple[MockState.Key, JsonPredicate.Type]]
      .map(jot => IsSet(jot.key, jot.value))
      .at("set")
      .widen
  )

  implicit val encoder: Encoder[StateTests] = Encoder.instance {
    case IsCleared(key)    => Json.obj("cleared" := key)
    case IsSet(key, value) => Json.obj("set" := JsonObjectTuple.json(key, value))
  }

  implicit val eq: Eq[StateTests] = Eq.instance {
    case (IsCleared(ka), IsCleared(kb)) => ka === kb
    case (IsSet(ka, va), IsSet(kb, vb)) => ka === kb && va === vb
    case _                              => false
  }

  implicit val checker: PredicateChecker[MockState.State, StateTests] = { (predicate, in) =>
    predicate match {
      case IsCleared(key)    => !MockState.State.raw(in).contains(key)
      case IsSet(key, value) =>
        MockState.State.raw(in).get(key).exists(_.satisfies(value))
    }
  }
}

object StatePredicate extends PredicateWrapper[MockState.State, StateTests |+| Fixed[MockState.State]] {
  val always: Type = wrap {
    Fixed
      .Always[MockState.State]()
      .upcast
      .second[StateTests]
      .first[UsingCombinators[Base]]
  }

  val never: Type = wrap {
    Fixed
      .Never[MockState.State]()
      .upcast
      .second[StateTests]
      .first[UsingCombinators[Base]]
  }

  def isCleared(key: MockState.Key): Type = wrap {
    StateTests
      .IsCleared(key)
      .upcast
      .first[Fixed[MockState.State]]
      .first[UsingCombinators[Base]]
  }

  def isSet(key: MockState.Key, value: JsonPredicate.Type): Type = wrap {
    StateTests
      .IsSet(key, value)
      .upcast
      .first[Fixed[MockState.State]]
      .first[UsingCombinators[Base]]
  }
}
