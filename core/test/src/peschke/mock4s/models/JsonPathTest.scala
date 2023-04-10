package peschke.mock4s.models

import cats.data.Chain
import cats.syntax.all._
import io.circe.Json
import io.circe.syntax._
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop._
import peschke.mock4s.JsonGens.{jsonBooleans, jsonNulls, jsonNumbers, jsonStrings}
import peschke.mock4s.MUnitCats
import peschke.mock4s.models.JsonPath.Segment
import peschke.mock4s.models.JsonPath.Segment.{AtIndex, BareField, DownArray}
import peschke.mock4s.utils.JsonPathParser

class JsonPathTest extends ScalaCheckSuite with MUnitCats {
  List[((Json, String), Option[Json])](
    Json.arr() -> ".[]" -> none,
    Json.arr() -> ".foo" -> none,
    Json.obj("foo" := 5) -> ".foo" -> 5.asJson.some,
    Json.obj("foo" := Json.obj("bar" := 5)) -> ".foo" -> Json.obj("bar" := 5).some,
    Json.obj("foo" := Json.arr(
      Json.obj("bar" := 1),
      2.asJson,
      Json.obj("baz" := 3),
      Json.obj("bar" := 4)
    )) -> ".foo[].bar" -> Json.arr(1.asJson, 4.asJson).some
  ).foreach {
    case ((input, pathStr), expected) =>
      val inputStr = input.noSpaces
      test(show"$inputStr <<: $pathStr") {
        val path = JsonPathParser.parse(pathStr).valueOr { e =>
          fail(s"Invalid path provided to test <$pathStr>:${e.mkString_("\n  ", "\n  ", "\n")}")
        }

        assertEq(input <<: path, expected)
      }
  }

  property("JsonPath.paths stops at scalar values") {
    val gen = Gen.oneOf(
      jsonNulls, jsonBooleans, jsonNumbers, jsonStrings
    )
    forAll(gen) { json =>
      assertEq(JsonPath.paths(json), Chain.one(JsonPath.Root))
    }
  }

  test("JsonPath.paths stops at empty arrays and objects") {
    assertEq(JsonPath.paths(Json.obj()), Chain.one(JsonPath.Root))
    assertEq(JsonPath.paths(Json.arr()), Chain.one(JsonPath.Root))
  }

  private val terminals = Gen.oneOf(
    jsonNulls, jsonBooleans, jsonNumbers, jsonStrings, Gen.const(Json.arr()), Gen.const(Json.obj())
  )

  property("JsonPath.paths follows all object keys") {
    val gen = Gen.resize(5, Gen.nonEmptyListOf(Gen.zip(Gen.alphaNumStr, terminals))).map(_.distinctBy(_._1))
    forAllNoShrink(gen) { keysAndTerminalsList =>
      val input = Json.obj(keysAndTerminalsList:_*)
      val expected = Chain.fromSeq(keysAndTerminalsList).map(_._1).map(Segment.of).map(JsonPath.of(_)).sorted
      assertEq(JsonPath.paths(input), expected,
        clue(
          show"""|input: $input
                 |expected: $expected
                 |""".stripMargin)
      )
    }
  }

  property("JsonPath.paths follows all vector indexes") {
    val gen = Gen.resize(5, Gen.nonEmptyListOf(terminals))
    forAllNoShrink(gen) { terminalsList =>
      val input = Json.arr(terminalsList: _*)
      val expected =
        Chain.fromSeq(terminalsList.indices)
          .map(Segment.of)
          .map(JsonPath.of(_))
          .sorted

      assertEq(JsonPath.paths(input), expected,
        clue(
          show"""|input: $input
                 |expected: $expected
                 |""".stripMargin)
      )
    }
  }

  test("JsonPath.paths produces the expected output in deeply nested cases") {
    val input = Json.arr(
      Json.obj(
        "a" := 1.asJson,
        "b" := Json.obj("c" := Json.obj(
          "d" := 2.asJson,
          "e" := Json.obj("f" := 3.asJson),
          "g" := Json.arr(
            4.asJson,
            Json.arr(
              5.asJson,
              6.asJson,
              Json.arr()
            )
          )
        )),
        "c" := Json.arr(
          Json.arr(Json.arr(Json.arr(7.asJson, 8.asJson, 9.asJson))),
          Json.arr(Json.arr(Json.arr(10.asJson, 11.asJson, 12.asJson))),
          Json.arr(Json.arr(Json.arr(13.asJson, 14.asJson, 15.asJson))),
          Json.arr(Json.arr(Json.arr(16.asJson, 17.asJson, 18.asJson)))
        ),
        "d" := Json.obj()
      )
    )
    val expected = Chain(
      JsonPath.of(AtIndex(0), BareField("a")),
      JsonPath.of(AtIndex(0), BareField("b"), BareField("c"), BareField("d")),
      JsonPath.of(AtIndex(0), BareField("b"), BareField("c"), BareField("e"), BareField("f")),
      JsonPath.of(AtIndex(0), BareField("b"), BareField("c"), BareField("g"), AtIndex(0)),
      JsonPath.of(AtIndex(0), BareField("b"), BareField("c"), BareField("g"), AtIndex(1), AtIndex(0)),
      JsonPath.of(AtIndex(0), BareField("b"), BareField("c"), BareField("g"), AtIndex(1), AtIndex(1)),
      JsonPath.of(AtIndex(0), BareField("b"), BareField("c"), BareField("g"), AtIndex(1), AtIndex(2)),
      JsonPath.of(AtIndex(0), BareField("c"), AtIndex(0), AtIndex(0), AtIndex(0), AtIndex(0)),
      JsonPath.of(AtIndex(0), BareField("c"), AtIndex(0), AtIndex(0), AtIndex(0), AtIndex(1)),
      JsonPath.of(AtIndex(0), BareField("c"), AtIndex(0), AtIndex(0), AtIndex(0), AtIndex(2)),
      JsonPath.of(AtIndex(0), BareField("c"), AtIndex(1), AtIndex(0), AtIndex(0), AtIndex(0)),
      JsonPath.of(AtIndex(0), BareField("c"), AtIndex(1), AtIndex(0), AtIndex(0), AtIndex(1)),
      JsonPath.of(AtIndex(0), BareField("c"), AtIndex(1), AtIndex(0), AtIndex(0), AtIndex(2)),
      JsonPath.of(AtIndex(0), BareField("c"), AtIndex(2), AtIndex(0), AtIndex(0), AtIndex(0)),
      JsonPath.of(AtIndex(0), BareField("c"), AtIndex(2), AtIndex(0), AtIndex(0), AtIndex(1)),
      JsonPath.of(AtIndex(0), BareField("c"), AtIndex(2), AtIndex(0), AtIndex(0), AtIndex(2)),
      JsonPath.of(AtIndex(0), BareField("c"), AtIndex(3), AtIndex(0), AtIndex(0), AtIndex(0)),
      JsonPath.of(AtIndex(0), BareField("c"), AtIndex(3), AtIndex(0), AtIndex(0), AtIndex(1)),
      JsonPath.of(AtIndex(0), BareField("c"), AtIndex(3), AtIndex(0), AtIndex(0), AtIndex(2)),
      JsonPath.of(AtIndex(0), BareField("d"))
    )
    val downArrayStyle = Chain(
      JsonPath.of(DownArray, BareField("a")),
      JsonPath.of(DownArray, BareField("b"), BareField("c"), BareField("d")),
      JsonPath.of(DownArray, BareField("b"), BareField("c"), BareField("e"), BareField("f")),
      JsonPath.of(DownArray, BareField("b"), BareField("c"), BareField("g"), DownArray),
      JsonPath.of(DownArray, BareField("b"), BareField("c"), BareField("g"), DownArray, DownArray),
      JsonPath.of(DownArray, BareField("c"), DownArray, DownArray, DownArray, DownArray),
      JsonPath.of(DownArray, BareField("d"))
    )
    assertEq(JsonPath.paths(input), expected)
    assertEq(JsonPath.downArrayStylePaths(input), downArrayStyle)
  }
}
