package peschke.mock4s.predicates

import io.circe.Json
import io.circe.syntax._
import munit._
import peschke.mock4s.MUnitCirce
import peschke.mock4s.models.JsonPath
import peschke.mock4s.models.JsonPath.Segment.BareField

class JsonPredicateJsonTest extends FunSuite with MUnitCirce {
  private val path = JsonPath.of(BareField("foo"), BareField("bar"))

  test("Always (no path)") {
    assertCodec(
      JsonPredicate.always,
      Json.obj("when" := "any")
    )
  }

  test("Always (with path)") {
    assertCodec(
      JsonPredicate.always(path),
      Json.obj(
        "path" := path,
        "when" := "any"
      )
    )
  }

  test("Never (no path)") {
    assertCodec(
      JsonPredicate.never,
      Json.obj("when" := "fail")
    )
  }

  test("Never (with path)") {
    assertCodec(
      JsonPredicate.never(path),
      Json.obj(
        "path" := path,
        "when" := "fail"
      )
    )
  }

  test("Is (no path)") {
    assertCodec(
      JsonPredicate.is(5.asJson),
      Json.obj("when" := Json.obj("is" := 5))
    )
  }

  test("Is (with path)") {
    assertCodec(
      JsonPredicate.is(path, 5.asJson),
      Json.obj(
        "path" := path,
        "when" := Json.obj("is" := 5)
      )
    )
  }

  test("In (no path)") {
    assertCodec(
      JsonPredicate.in(5.asJson :: 6.asJson :: Nil),
      Json.obj("when" := Json.obj("in" := Json.arr(5.asJson, 6.asJson)))
    )
  }

  test("In (with path)") {
    assertCodec(
      JsonPredicate.in(path, 5.asJson :: 6.asJson :: Nil),
      Json.obj(
        "path" := path,
        "when" := Json.obj("in" := Json.arr(5.asJson, 6.asJson))
      )
    )
  }

  test("Not (no path)") {
    assertCodec(
      JsonPredicate.not(JsonPredicate.is(5.asJson)),
      Json.obj("!" := Json.obj("when" := Json.obj("is" := 5)))
    )
  }

  test("Not (with path)") {
    assertCodec(
      JsonPredicate.not(JsonPredicate.is(path, 5.asJson)),
      Json.obj("!" := Json.obj(
        "path" := path,
        "when" := Json.obj("is" := 5)
      ))
    )
  }
}
