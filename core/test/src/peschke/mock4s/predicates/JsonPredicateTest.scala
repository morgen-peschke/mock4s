package peschke.mock4s.predicates

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
import peschke.mock4s.predicates.JsonPredicate.{always, in, is, never}

class JsonPredicateTest extends ScalaCheckSuite with MUnitPredicateAsserts with MUnitCats {
  private val jsons = Gen.resize(5, configs.flatMap(_.mkGen))

  property("always() should accept any JSON") {
    forAll(jsons) { json =>
      assertAccepts(always, json)
    }
  }

  property("always(path) should accept any JSON that has something at that path") {
    forAll(withPath(jsons)) { case (json, path, _) =>
      assertAccepts(always(path), json)
    }
  }

  property("always(path) should reject any JSON that has nothing at that path") {
    forAll(withoutPath(jsons)) { case (json, path, _) =>
      assertRejects(always(path), json)
    }
  }

  property("never() should reject all JSON") {
    forAll(jsons) {
      assertRejects(never, _)
    }
  }

  property("never(path) should accept json without something at that path") {
    forAll(withoutPath(jsons)) { case (json, path, _) =>
      assertAccepts(never(path), json)
    }
  }

  property("never(path) should reject json with something at that path") {
    forAllNoShrink(withPath(jsons)) { case (json, path, _) =>
      assertRejects(never(path), json)
    }
  }

  property("is(_) should accept if the whole JSON matches") {
    forAll(jsons) { json =>
      assertAccepts(is(json), json)
    }
  }

  property("is(path, _) should reject json with nothing at that path") {
    forAll(withoutPath(jsons)) { case (json, path, oldValue) =>
      assertRejects(is(path, oldValue), json)
    }
  }

  property("is(path, _) should accept if the JSON value returned by the path matches") {
    forAll(withPath(jsons)) { case (json, path, sentinel) =>
      assertAccepts(is(path, sentinel), json)
    }
  }

  test("is(path, _) should accept if any of the JSON values returned by the path matches") {
    val input = Json.obj(
      "a" := Json.arr(
        Json.obj("b" := 1.asJson),
        Json.obj("b" := 2.asJson),
        Json.obj("b" := 3.asJson)
      )
    )
    assertAccepts(is(Root / "a" / DownArray / "b", 1.asJson), input)
    assertAccepts(is(Root / "a" / DownArray / "b", 2.asJson), input)
    assertAccepts(is(Root / "a" / DownArray / "b", 3.asJson), input)
    assertRejects(is(Root / "a" / DownArray / "b", 4.asJson), input)
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

  property("in(path, _) should reject json with nothing at that path") {
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
      assertRejects(in(path, sentinels), json)
    }
  }

  test("in(path, _) should accept if the JSON value returned by the path is one of the specified values") {
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
      assertAccepts(in(path, sentinels), json)
    }
  }

  test("in(path, _) should accept if any of the JSON values returned by the path is one of the specified values") {
    val input = Json.obj(
      "a" := Json.arr(
        Json.obj("b" := 1.asJson),
        Json.obj("b" := 2.asJson),
        Json.obj("b" := 3.asJson)
      )
    )
    assertAccepts(in(Root / "a" / DownArray / "b", 1.asJson :: 20.asJson :: Nil), input)
    assertAccepts(in(Root / "a" / DownArray / "b", 2.asJson :: 20.asJson :: Nil), input)
    assertAccepts(in(Root / "a" / DownArray / "b", 3.asJson :: 20.asJson :: Nil), input)
    assertRejects(in(Root / "a" / DownArray / "b", 4.asJson :: 20.asJson :: Nil), input)
  }
}
