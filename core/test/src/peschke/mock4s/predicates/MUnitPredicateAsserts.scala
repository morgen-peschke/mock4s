package peschke.mock4s.predicates

import cats.Show
import cats.syntax.all._
import io.circe.Encoder
import io.circe.syntax._
import munit.{Assertions, Clue, Location}

trait MUnitPredicateAsserts { self: Assertions =>
  def assertAccepts[A: Show, PA <: Predicate[A]: Encoder](predicate: PA, value: Clue[A])
                                                         (implicit location: Location): Unit = {
    if (!predicate.test(value.value)) {
      fail(show"${predicate.asJson.noSpaces}.test(${value.value}) != true")(location)
    }
  }

  def assertRejects[A: Show, PA <: Predicate[A]: Encoder](predicate: PA, value: Clue[A])
                                                         (implicit location: Location): Unit = {
    if (predicate.test(value.value)) {
      fail(show"${predicate.asJson.noSpaces}.test(${value.value}) != false")(location)
    }
  }
}
