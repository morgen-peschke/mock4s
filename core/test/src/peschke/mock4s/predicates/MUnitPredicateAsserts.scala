package peschke.mock4s.predicates

import cats.Show
import cats.syntax.all._
import io.circe.Encoder
import io.circe.syntax._
import munit.Assertions
import munit.Clue
import munit.Location
import peschke.mock4s.algebras.PredicateChecker
import peschke.mock4s.algebras.PredicateChecker.syntax._

trait MUnitPredicateAsserts { self: Assertions =>
  def assertAccepts[In: Show, ADT: Encoder]
    (predicate:         ADT, value:        Clue[In])
    (implicit location: Location, checker: PredicateChecker[In, ADT])
    : Unit = {
    if (!predicate.satisfiedBy(value.value)) {
      fail(show"${predicate.asJson.noSpaces}.test(${value.value}) != true")(location)
    }
  }

  def assertRejects[In: Show, ADT: Encoder]
    (predicate:         ADT, value:        Clue[In])
    (implicit location: Location, checker: PredicateChecker[In, ADT])
    : Unit = {
    if (predicate.satisfiedBy(value.value)) {
      fail(show"${predicate.asJson.noSpaces}.test(${value.value}) != false")(location)
    }
  }
}
