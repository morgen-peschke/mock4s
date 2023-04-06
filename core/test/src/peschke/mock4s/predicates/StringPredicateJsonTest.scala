package peschke.mock4s.predicates

import io.circe.Json
import io.circe.syntax._
import munit._
import peschke.mock4s.MUnitCirce

class StringPredicateJsonTest extends FunSuite with MUnitCirce {
  import StringPredicate.{startsWith, endsWith, contains, matches}

  test("StartsWith") {
    assertCodec(
      startsWith("some arbitrary prefix"),
      Json.obj("starts-with" := "some arbitrary prefix")
    )
  }

  test("EndsWith") {
    assertCodec(
      endsWith("some arbitrary suffix"),
      Json.obj("ends-with" := "some arbitrary suffix")
    )
  }

  test("Contains") {
    assertCodec(
      contains("some arbitrary prefix"),
      Json.obj("contains" := "some arbitrary prefix")
    )
  }

  test("Matches") {
    assertCodec(
      matches(".*".r),
      Json.obj("matches" := ".*")
    )
  }
}
