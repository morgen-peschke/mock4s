package peschke.mock4s.predicates

import cats.data.Chain
import io.circe.Json
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary.arbitrary
import io.circe.syntax._
import peschke.mock4s.JsonGens.{configs, terminals, withPath, withoutPath}
import peschke.mock4s.MUnitCats
import peschke.mock4s.models.JsonPath.Root
import peschke.mock4s.models.JsonPath.Segment.DownArray
import peschke.mock4s.predicates.JsonPredicate.{always, atPath, in, is, never, string}

class JsonPredicateTest extends ScalaCheckSuite with MUnitPredicateAsserts with MUnitCats {
  private val jsons = Gen.resize(5, configs.flatMap(_.mkGen))

  property("always should accept any JSON") {
    forAll(jsons) { json =>
      assertAccepts(always, json)
    }
  }

  property("atPath(path, always) should accept any JSON that has something at that path") {
    forAll(withPath(jsons)) { case (json, path, _) =>
      assertAccepts(atPath(path, always), json)
    }
  }

  property("atPath(path, always) should reject any JSON that has nothing at that path") {
    forAll(withoutPath(jsons)) { case (json, path, _) =>
      assertRejects(atPath(path, always), json)
    }
  }

  property("never should reject all JSON") {
    forAll(jsons) {
      assertRejects(never, _)
    }
  }

  property("atPath(path, never) should reject json without something at that path") {
    forAll(withoutPath(jsons)) { case (json, path, _) =>
      assertRejects(atPath(path, never), json)
    }
  }

  property("atPath(path, never) should reject json with something at that path") {
    forAllNoShrink(withPath(jsons)) { case (json, path, _) =>
      assertRejects(atPath(path, never), json)
    }
  }

  property("is(_) should accept if the whole JSON matches") {
    forAll(jsons) { json =>
      assertAccepts(is(json), json)
    }
  }

  property("atPath(path, is(_)) should reject json with nothing at that path") {
    forAll(withoutPath(jsons)) { case (json, path, oldValue) =>
      assertRejects(atPath(path, is(oldValue)), json)
    }
  }

  property("atPath(path, is(_)) should accept if the JSON value returned by the path matches") {
    forAll(withPath(jsons)) { case (json, path, sentinel) =>
      assertAccepts(atPath(path, is(sentinel)), json)
    }
  }

  test("atPath(path, is(_)) should accept if any of the JSON values returned by the path matches") {
    val input = Json.obj(
      "a" := Json.arr(
        Json.obj("b" := 1.asJson),
        Json.obj("b" := 2.asJson),
        Json.obj("b" := 3.asJson)
      )
    )
    assertAccepts(atPath(Root / "a" / DownArray / "b", is(1.asJson)), input)
    assertAccepts(atPath(Root / "a" / DownArray / "b", is(2.asJson)), input)
    assertAccepts(atPath(Root / "a" / DownArray / "b", is(3.asJson)), input)
    assertRejects(atPath(Root / "a" / DownArray / "b", is(4.asJson)), input)
  }

  test("in(_) should accept if the JSON value is one of the specified values") {
    val gen = for {
      json           <- jsons
      jsonShuffleKey <- arbitrary[Int]
      otherSentinels <- Gen.resize(10, Gen.listOf(Gen.zip(terminals, arbitrary[Int])))
    } yield {
      val sentinels =
        ((json, jsonShuffleKey) :: otherSentinels).sortBy(_._2).map(_._1)

      (json, sentinels)
    }
    forAll(gen) { case (json, sentinels) =>
      assertAccepts(in(sentinels), json)
    }
  }

  property("atPath(path, in(_)) should reject json with nothing at that path") {
    val gen = for {
      (json, path, sentinel) <- withoutPath(jsons)
      jsonShuffleKey         <- arbitrary[Int]
      otherSentinels         <- Gen.resize(10, Gen.listOf(Gen.zip(terminals, arbitrary[Int])))
    } yield {
      val sentinels =
        ((sentinel, jsonShuffleKey) :: otherSentinels).sortBy(_._2).map(_._1)

      (json, path, sentinels)
    }
    forAll(gen) { case (json, path, sentinels) =>
      assertRejects(atPath(path, in(sentinels)), json)
    }
  }

  test("atPath(path, in(_)) should accept if the JSON value returned by the path is one of the specified values") {
    val gen = for {
      (json, path, sentinel) <- withPath(jsons)
      jsonShuffleKey         <- arbitrary[Int]
      otherSentinels         <- Gen.resize(10, Gen.listOf(Gen.zip(terminals, arbitrary[Int])))
    } yield {
      val sentinels =
        ((sentinel, jsonShuffleKey) :: otherSentinels).sortBy(_._2).map(_._1)

      (json, path, sentinels)
    }
    forAll(gen) { case (json, path, sentinels) =>
      assertAccepts(atPath(path, in(sentinels)), json)
    }
  }

  test("atPath(path, in(_)) should accept if any of the JSON values returned by the path is one of the specified values") {
    val input = Json.obj(
      "a" := Json.arr(
        Json.obj("b" := 1.asJson),
        Json.obj("b" := 2.asJson),
        Json.obj("b" := 3.asJson)
      )
    )
    assertAccepts(atPath(Root / "a" / DownArray / "b", in(1.asJson :: 20.asJson :: Nil)), input)
    assertAccepts(atPath(Root / "a" / DownArray / "b", in(2.asJson :: 20.asJson :: Nil)), input)
    assertAccepts(atPath(Root / "a" / DownArray / "b", in(3.asJson :: 20.asJson :: Nil)), input)
    assertRejects(atPath(Root / "a" / DownArray / "b", in(4.asJson :: 20.asJson :: Nil)), input)
  }

  test("string(_) should accept if the JSON value is a string that satisfies the predicate") {
    forAll { str: String =>
      assertAccepts(string(StringPredicate.is(str)), Json.fromString(str))
    }
  }

  test("atPath(path, string(_)) should accept if the JSON value is a string that satisfies the predicate") {
    forAll(withPath(jsons), arbitrary[String]) { case ((json, path, _), str) =>
      val updatedJson = assertSome {
        path.hCursors(json).map(_.withFocus(_ => Json.fromString(str)).root.focus).flatMap(Chain.fromOption).headOption
      }
      assertAccepts(atPath(path, string(StringPredicate.is(str))), updatedJson)
    }
  }

  test("atPath(path, string(_)) should reject if the JSON value is a string that does not satisfy the predicate") {
    forAll(withPath(jsons), arbitrary[String]) { case ((json, path, _), str) =>
      val updatedJson = assertSome {
        path.hCursors(json).map(_.withFocus(_ => Json.fromString(str)).root.focus).flatMap(Chain.fromOption).headOption
      }
      assertRejects(atPath(path, string(StringPredicate.is(s"$str-"))), updatedJson)
    }
  }

  test("atPath(path, string(_)) should reject if the JSON value is not a string") {
    forAll(
      withPath(jsons),
      arbitrary[String],
      jsons
    ) { case ((json, path, _), str, replacement) =>
      val replacementJson = if (replacement.isString) Json.Null else replacement
      val updatedJson = assertSome {
        path.hCursors(json).map(_.withFocus(_ => replacementJson).root.focus).flatMap(Chain.fromOption).headOption
      }
      assertRejects(atPath(path, string(StringPredicate.is(str))), updatedJson)
    }
  }
}
