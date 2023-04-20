package peschke.mock4s.predicates

import io.circe.Json
import io.circe.syntax._
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop._
import peschke.mock4s.models.MockState
import peschke.mock4s.predicates.StatePredicate.{isCleared, isSet}
import peschke.mock4s.{JsonGens, MUnitCirce}

class StatePredicateJsonTest extends ScalaCheckSuite with MUnitCirce {
  property("IsCleared") {
    forAll(Gen.alphaNumStr) { key =>
      assertCodec(
        isCleared(MockState.Key(key)),
        Json.obj("cleared" := key)
      )
    }
  }

  property("IsSet") {
    forAll(Gen.alphaNumStr, JsonGens.scalars.map(JsonPredicate.is)){ (key, value) =>
      assertCodec(
        isSet(MockState.Key(key), value),
        Json.obj("set" := Json.obj(key := value))
      )
    }
  }
}
