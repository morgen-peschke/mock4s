package peschke.mock4s.predicates

import io.circe.Json
import io.circe.syntax._
import munit._
import peschke.mock4s.MUnitCirce
import peschke.mock4s.predicates.Predicate.{Fixed, UsingCombinators, UsingEq, UsingOrder}

class PredicateJsonTest extends FunSuite with MUnitCirce {
  test("Fixed.Always") {
    assertCodec(Fixed.Always[String](), "any".asJson)
  }

  test("Fixed.Never") {
    assertCodec(Fixed.Never[String](), "fail".asJson)
  }

  test("UsingEq.Is") {
    assertCodec(UsingEq.Is(5), Json.obj("is" := 5))
  }

  test("UsingEq.In") {
    assertCodec(UsingEq.In(List(1, 2, 3)), Json.obj("in" := List(1, 2, 3)))
  }

  test("UsingOrder.LessThan") {
    assertCodec(UsingOrder.LessThan(5), Json.obj("<" := 5))
  }

  test("UsingOrder.LessThanEq") {
    assertCodec(UsingOrder.LessThanEq(5), Json.obj("<=" := 5))
  }

  test("UsingOrder.GreaterThan") {
    assertCodec(UsingOrder.GreaterThan(5), Json.obj(">" := 5))
  }

  test("UsingOrder.GreaterThanEq") {
    assertCodec(UsingOrder.GreaterThanEq(5), Json.obj(">=" := 5))
  }

  test("UsingCombinators.Wrapped") {
    assertCodec[UsingCombinators[Int, UsingEq[Int]]](
      UsingCombinators.Wrapped[Int, UsingEq[Int]](UsingEq.In(List(1, 2, 3))),
      Json.obj("in" := List(1, 2, 3))
    )
  }

  test("UsingCombinators.Not") {
    assertCodec[UsingCombinators[Int, UsingEq[Int]]](
      UsingCombinators.Not[Int, UsingEq[Int]](
        UsingCombinators.Wrapped(UsingEq.In(List(1, 2, 3)))
      ),
      Json.obj("!" := Json.obj("in" := List(1, 2, 3)))
    )
  }

  test("UsingCombinators.ForAll") {
    assertCodec[UsingCombinators[Int, UsingEq[Int]]](
      UsingCombinators.ForAll[Int, UsingEq[Int]](
        List(
          UsingCombinators.Wrapped[Int, UsingEq[Int]](UsingEq.In(List(1, 2, 3))),
          UsingCombinators.Wrapped[Int, UsingEq[Int]](UsingEq.Is(7))
        )
      ),
      Json.obj(
        "forall" := List(
          Json.obj("in" := List(1, 2, 3)),
          Json.obj("is" := 7)
        )
      )
    )
  }

  test("UsingCombinators.Exists") {
    assertCodec[UsingCombinators[Int, UsingEq[Int]]](
      UsingCombinators.Exists[Int, UsingEq[Int]](
        List(
          UsingCombinators.Wrapped[Int, UsingEq[Int]](UsingEq.In(List(1, 2, 3))),
          UsingCombinators.Wrapped[Int, UsingEq[Int]](UsingEq.Is(7))
        )
      ),
      Json.obj(
        "exists" := List(
          Json.obj("in" := List(1, 2, 3)),
          Json.obj("is" := 7)
        )
      )
    )
  }

  object SimpleCharPred extends Predicate.SimpleEq[Char]

  test("Predicate.SimpleEq") {
    import SimpleCharPred.{always, exists, forall, in, is, never, not}
    assertCodec(
      exists(
        List(
          is('a'),
          in('a' :: 'b' :: 'c' :: Nil),
          not(never),
          forall(always :: not(never) :: is('a') :: Nil)
        )
      ),
      Json.obj(
        "exists" := Json.arr(
          Json.obj("is" := 'a'),
          Json.obj("in" := List('a', 'b', 'c')),
          Json.obj("!"  := "fail"),
          Json.obj(
            "forall"    := Json.arr(
              "any".asJson,
              Json.obj("!"  := "fail"),
              Json.obj("is" := 'a'.asJson)
            )
          )
        )
      )
    )
  }

  object SimpleIntPred extends Predicate.SimpleOrder[Int]

  test("Predicate.SimpleOrder") {
    import SimpleIntPred.{always, exists, forall, greaterThan, greaterThanEq, in, is, lessThan, lessThanEq, never, not}
    assertCodec(
      exists(
        List(
          is(1),
          in(1 :: 2 :: 3 :: Nil),
          lessThan(1),
          lessThanEq(2),
          greaterThan(4),
          greaterThanEq(5),
          not(never),
          forall(always :: not(never) :: is(1) :: Nil)
        )
      ).asJson,
      Json.obj(
        "exists" := Json.arr(
          Json.obj("is" := 1),
          Json.obj("in" := List(1, 2, 3)),
          Json.obj("<"  := 1),
          Json.obj("<=" := 2),
          Json.obj(">"  := 4),
          Json.obj(">=" := 5),
          Json.obj("!"  := "fail"),
          Json.obj(
            "forall"    := Json.arr(
              "any".asJson,
              Json.obj("!"  := "fail"),
              Json.obj("is" := 1.asJson)
            )
          )
        )
      )
    )
  }
}
