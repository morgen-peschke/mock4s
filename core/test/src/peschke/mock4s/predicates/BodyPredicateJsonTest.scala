package peschke.mock4s.predicates

import io.circe.Json
import io.circe.syntax._
import munit.FunSuite
import peschke.mock4s.MUnitCirce
import peschke.mock4s.models.Body.HexString
import peschke.mock4s.predicates.BodyPredicate.{isEmpty, json, raw, text}

class BodyPredicateJsonTest extends FunSuite with MUnitCirce {
  test("IsEmpty") {
    assertCodec(
      isEmpty,
      "empty".asJson
    )
  }

  test("TextBody") {
    assertCodec(
      text(StringPredicate.is("arbitrary text")),
      Json.obj("text" := Json.obj("is" := "arbitrary text"))
    )
  }

  test("JsonBody") {
    assertCodec(
      json(JsonPredicate.is(Json.obj("arbitrary json" := true))),
      Json.obj("json" := Json.obj("when" := Json.obj("is" := Json.obj("arbitrary json" := true))))
    )
  }

  test("RawBody") {
    assertCodec(
      raw(HexStringPredicate.is(HexString(BigInt(123456)))),
      Json.obj("raw" := Json.obj("is" := "1e240"))
    )
  }
}
