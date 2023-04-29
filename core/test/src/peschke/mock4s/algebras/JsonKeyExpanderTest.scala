package peschke.mock4s.algebras

import cats.Id
import cats.data.Chain
import io.circe.Json
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop._
import peschke.mock4s.JsonGens._
import peschke.mock4s.MUnitCats
import peschke.mock4s.models.JsonPath
import peschke.mock4s.models.JsonPath.Segment
import peschke.mock4s.models.JsonPath.Segment.DownArray
import peschke.mock4s.utils.JsonPathParserTest

class JsonKeyExpanderTest extends ScalaCheckSuite with MUnitCats {
  private val expander = JsonKeyExpander.default[Id]

  private val bareFields = JsonPathParserTest.bareFields.map(_._2)
  private val quotedFields = JsonPathParserTest.quotedFields.map(_._2)
  private val atIndexes = JsonPathParserTest.atIndexes.map(_._2)

  private val fields: Gen[Segment.FieldSegment] = Gen.oneOf(bareFields, quotedFields)

  property("expand should not change scalar values") {
    forAll(scalars) { json =>
      assertEq(expander.expand(json), json)
    }
  }

  property("expand should not change arrays") {
    forAll(arrays(JsonGenConfig(0, 0, 0, 0, objectKeys, scalars))) { json =>
      assertEq(expander.expand(json), json)
    }
  }

  property("expand should not change objects that do not have keys which are JsonPaths") {
    forAll(Gen.resize(5, configs).flatMap(_.mkGen)) { json =>
      assertEq(expander.expand(json), json)
    }
  }

  property("expand should not expand keys with paths that are empty") {
    val emptyKeys = Gen.oneOf(
      Gen.const("$"),
      Gen.const(""),
      Gen.alphaNumStr
    )
    forAll(Gen.resize(5, configs(emptyKeys, scalars)).flatMap(_.mkGen)) { json =>
      assertEq(expander.expand(json), json)
    }
  }

  property("expand should not expand keys with paths that include array index segments") {
    val keys = for {
      prefix <- Gen.chooseNum(0, 10).flatMap(Gen.listOfN(_, fields))
      middle <- atIndexes
      suffix <- Gen.chooseNum(0, 10).flatMap(Gen.listOfN(_, fields))
    } yield JsonPath.fromChain(Chain.fromSeq(prefix) ++ Chain.one(middle) ++ Chain.fromSeq(suffix)).render

    forAll(Gen.resize(5, configs(keys, scalars)).flatMap(_.mkGen)) { json =>
      assertEq(expander.expand(json), json)
    }
  }

  property("expand should expand field expansions (n = 1") {
    forAll(fields, scalars) {
      case (s0, json) =>
      assertEq(
        expander.expand(Json.obj(JsonPath.of(s0).render -> json)),
        Json.obj(s0.name -> json)
      )
    }
  }

  test("expand should expand nested field expansions (n = 2)") {
    forAll(fields, fields, scalars) {
      case (s0, s1, json) =>
        assertEq(
          expander.expand(Json.obj(JsonPath.of(s0, s1).render -> json)),
          Json.obj(s0.name -> Json.obj(s1.name -> json))
        )
    }
  }

  test("expand should expand nested field expansions (n = 3)") {
    forAll(fields, fields, fields, scalars) {
      case (s0, s1, s2, json) =>
        assertEq(
          expander.expand(Json.obj(JsonPath.of(s0, s1, s2).render -> json)),
          Json.obj(s0.name -> Json.obj(s1.name -> Json.obj(s2.name -> json)))
        )
    }
  }

  test("expand should expand nested field expansions (n = 4)") {
    forAll(fields, fields, fields, fields, scalars) {
      case (s0, s1, s2, s3, json) =>
        assertEq(
          expander.expand(Json.obj(JsonPath.of(s0, s1, s2, s3).render -> json)),
          Json.obj(s0.name -> Json.obj(s1.name -> Json.obj(s2.name -> Json.obj(s3.name -> json))))
        )
    }
  }

  test("expand should expand down array segments in the first location (l = 0)") {
    forAll(fields, fields, fields, fields, scalars) {
      case (s0, s1, s2, s3, value) =>
        val json = Json.obj(JsonPath.of(DownArray, s0, s1, s2, s3).render -> value)
        assertEq(expander.expand(json), json)
    }
  }

  test("expand should expand down array segments (l = 1)") {
    forAll(fields, fields, fields, fields, scalars) {
      case (s0, s1, s2, s3, json) =>
        assertEq(
            expander.expand(Json.obj(JsonPath.of(s0, DownArray, s1, s2, s3).render -> json)),
            Json.obj(s0.name -> Json.arr(Json.obj(s1.name -> Json.obj(s2.name -> Json.obj(s3.name -> json)))))
        )
    }
  }

  test("expand should expand down array segments (l = 2)") {
    forAll(fields, fields, fields, fields, scalars) {
      case (s0, s1, s2, s3, json) =>
        assertEq(
          expander.expand(Json.obj(JsonPath.of(s0, s1, DownArray, s2, s3).render -> json)),
          Json.obj(s0.name -> Json.obj(s1.name -> Json.arr(Json.obj(s2.name -> Json.obj(s3.name -> json)))))
        )
    }
  }

  test("expand should expand down array segments (l = 3)") {
    forAll(fields, fields, fields, fields, scalars) {
      case (s0, s1, s2, s3, json) =>
        assertEq(
          expander.expand(Json.obj(JsonPath.of(s0, s1, s2, DownArray, s3).render -> json)),
          Json.obj(s0.name -> Json.obj(s1.name -> Json.obj(s2.name -> Json.arr(Json.obj(s3.name -> json)))))
        )
    }
  }

  test("expand should expand down array segments (l = 4)") {
    forAll(fields, fields, fields, fields, scalars) {
      case (s0, s1, s2, s3, json) =>
        assertEq(
          expander.expand(Json.obj(JsonPath.of(s0, s1, s2, s3, DownArray).render -> json)),
          Json.obj(s0.name -> Json.obj(s1.name -> Json.obj(s2.name -> Json.obj(s3.name -> Json.arr(json)))))
        )
    }
  }
}
