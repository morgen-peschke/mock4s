package peschke.mock4s.predicates

import io.circe.Json
import io.circe.syntax._
import munit._
import peschke.mock4s.MUnitCirce
import peschke.mock4s.models.JsonPath
import peschke.mock4s.models.JsonPath.Segment.BareField
import peschke.mock4s.predicates.JsonPredicate.{always, atPath, in, is, never, not, string}
import peschke.mock4s.predicates.StringPredicate.startsWith

class JsonPredicateJsonTest extends FunSuite with MUnitCirce {
  private val path = JsonPath.of(BareField("foo"), BareField("bar"))

  test("Always (no path)") {
    assertCodec(always, "any".asJson)
    assertDecodes("always".asJson, always)
  }

  test("Always (with path)") {
    assertCodec(atPath(path, always), Json.obj("at" := path, "when" := "any"))
    assertDecodes(Json.obj("at" := path, "when" := "always"), atPath(path, always))
  }

  test("Never (no path)") {
    assertCodec(never, "fail".asJson)
    assertDecodes("never".asJson, never)
  }

  test("Never (with path)") {
    assertCodec(atPath(path, never), Json.obj("at" := path, "when" := "fail"))
    assertDecodes(Json.obj("at" := path, "when" := "never"), atPath(path, never))
  }

  test("Is (no path)") {
    assertCodec(is(5.asJson), Json.obj("is" := 5))
  }

  test("Is (with path)") {
    assertCodec(
      atPath(path, is(5.asJson)),
      Json.obj("at" := path, "when" := Json.obj("is" := 5))
    )
  }

  test("In (no path)") {
    assertCodec(in(5.asJson :: 6.asJson :: Nil), Json.obj("in" := Json.arr(5.asJson, 6.asJson)))
  }

  test("In (with path)") {
    assertCodec(
      atPath(path, in(5.asJson :: 6.asJson :: Nil)),
      Json.obj(
        "at" := path,
        "when" := Json.obj("in" := Json.arr(5.asJson, 6.asJson))
      )
    )
  }

  test("Not (no path)") {
    assertCodec(not(is(5.asJson)), Json.obj("!" := Json.obj("is" := 5)))
  }

  test("Not (with path)") {
    assertCodec(
      atPath(path, not(is(5.asJson))),
      Json.obj(
        "!" := Json.obj(
          "at" := path,
          "when" := Json.obj("is" := 5)
        )
      )
    )
  }

  test("Not (with path twice)") {
    assertCodec(
      atPath(path, not(atPath(path, is(5.asJson)))),
      Json.obj(
        "at" := path,
        "when" := Json.obj(
          "!" := Json.obj(
            "at" := path,
            "when" := Json.obj("is" := 5)
          )
        )
      )
    )
  }

  test("IsString (no path)") {
    assertCodec(
      string(startsWith("prefix")),
      Json.obj("string" := Json.obj("starts-with" := "prefix"))
    )
  }

  test("IsString (with path)") {
    assertCodec(
      atPath(path, string(startsWith("prefix"))),
      Json.obj("at" := path, "when" := Json.obj("string" := Json.obj("starts-with" := "prefix")))
    )
  }
}
