package peschke.mock4s.predicates

import io.circe.Json
import io.circe.syntax._
import munit._
import org.typelevel.ci._
import peschke.mock4s.MUnitCirce

class HeaderPredicateJsonTest extends FunSuite with MUnitCirce {
  test("BaseHeaderPred") {
    assertCodec(
      HeaderPredicate.header(ci"some-header", StringPredicate.is("some-value")),
      Json.obj("some-header" := Json.obj("is" := "some-value"))
    )
  }

  test("not") {
    assertCodec(
      HeaderPredicate.not(HeaderPredicate.header(ci"name", StringPredicate.is("value"))),
      Json.obj("!" := Json.obj("name" := Json.obj("is" := "value")))
    )
  }

  test("forall") {
    assertCodec(
      HeaderPredicate.forall(List(
        HeaderPredicate.header(ci"name", StringPredicate.is("value")),
        HeaderPredicate.not(HeaderPredicate.header(ci"name", StringPredicate.is("value")))
      )),
      Json.obj("forall" := List(
        Json.obj("name" := Json.obj("is" := "value")),
        Json.obj( "!" := Json.obj("name" := Json.obj("is" := "value"))))
      )
    )
  }
}
