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
      Json.obj(
        "name"  := "some-header",
        "value" := Json.obj("is" := "some-value")
      )
    )
  }
}
