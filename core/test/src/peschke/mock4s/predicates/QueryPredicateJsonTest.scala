package peschke.mock4s.predicates

import io.circe.Json
import io.circe.syntax._
import munit.FunSuite
import peschke.mock4s.MUnitCirce

class QueryPredicateJsonTest extends FunSuite with MUnitCirce {
  test("Empty") {
    assertCodec(
      QueryPredicate.empty,
      "empty".asJson
    )
  }

  test("OmitsParam") {
    assertCodec(
      QueryPredicate.omitsParam("key-name"),
      Json.obj("omits" := "key-name")
    )
  }

  test("EmptyParam") {
    assertCodec(
      QueryPredicate.emptyParam("key-name"),
      Json.obj("empty-param" := "key-name")
    )
  }

  test("ForParam") {
    assertCodec(
      QueryPredicate.forParam("key-name", StringPredicate.startsWith("prefix")),
      Json.obj("param" := Json.obj("key-name" := Json.obj("starts-with" := "prefix")))
    )
  }
}
