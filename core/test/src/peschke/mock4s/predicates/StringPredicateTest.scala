package peschke.mock4s.predicates

import cats.Eq
import cats.syntax.all._
import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import peschke.mock4s.predicates.StringPredicate.{always, contains, endsWith, in, is, never, startsWith}
import peschke.mock4s.predicates.StringPredicateTest.{unique2, valueInNonEmptyList, valueNotInNonEmptyList}

import scala.annotation.tailrec

class StringPredicateTest extends ScalaCheckSuite with MUnitPredicateAsserts {
  property("always should accept all input") {
    forAll(arbitrary[String])(assertAccepts(always, _))
  }

  property("never should reject all input") {
    forAll(arbitrary[String])(assertRejects(never, _))
  }

  property("is should accept exactly equal strings") {
    forAll(arbitrary[String])(s => assertAccepts(is(s), s))
  }

  property("is should reject unequal strings") {
    forAll(unique2[String](arbitrary[String], s => s"$s-")) {
      case (a, b) => assertRejects(is(a), b)
    }
  }

  property("in should accept strings in the list") {
    forAll(valueInNonEmptyList(arbitrary[String])) {
      case (a, list) => assertAccepts(in(list), a)
    }
  }

  property("in should reject everything if the list is empty") {
    forAll(arbitrary[String])(s => assertRejects(in(Nil), s))
  }

  property("in should reject strings not in the list") {
    forAll(valueNotInNonEmptyList[String](arbitrary[String], s => s"$s-")){
      case (a, b) => assertRejects(in(b), a)
    }
  }

  property("startsWith should accept strings with the prefix") {
    forAll(arbitrary[String], arbitrary[String]) { (a,b) =>
      assertAccepts(startsWith(a), s"$a$b")
    }
  }

  property("startsWith should reject strings without the prefix") {
    forAll(arbitrary[String], arbitrary[String]) { (a, b) =>
      val str =
        if (!b.startsWith(a)) s"$a$b"
        else if (a.startsWith("-")) s"=$a$b"
        else s"-$a$b"
      assertAccepts(startsWith(a), str)
    }
  }

  property("endsWith should accept strings with the suffix") {
    forAll(arbitrary[String], arbitrary[String]) { (a, b) =>
      assertAccepts(endsWith(b), s"$a$b")
    }
  }

  property("endsWith should reject strings without the suffix") {
    forAll(arbitrary[String], arbitrary[String]) { (a, b) =>
      val str =
        if (!a.endsWith(b)) s"$a$b"
        else if (a.endsWith("-")) s"$a$b="
        else s"$a$b-"
      assertAccepts(endsWith(b), str)
    }
  }

  property("contains should accept strings with the substring") {
    forAll(arbitrary[String], arbitrary[String], arbitrary[String]) { (a, b, c) =>
      assertAccepts(contains(a), s"$a$b$c")
      assertAccepts(contains(b), s"$a$b$c")
      assertAccepts(contains(c), s"$a$b$c")
    }
  }
}
object StringPredicateTest {
  def excluding[A: Eq](value: A, gen: Gen[A], munge: A => A): Gen[A] =
    gen.map { a =>
      if (a === value) munge(a) else a
    }

  def unique2[A: Eq](gen: Gen[A], munge: A => A): Gen[(A, A)] =
    for {
      a <- gen
      b <- excluding(a, gen, munge)
    } yield (a, b)

  def valueInNonEmptyList[A](gen: Gen[A]): Gen[(A, List[A])] =
    for {
      list <- Gen.nonEmptyListOf(gen)
      a <- Gen.oneOf(list)
    } yield (a, list)

  def valueNotInNonEmptyList[A](gen: Gen[A], munge: A => A): Gen[(A, List[A])] =
    for {
      list <- Gen.nonEmptyListOf(gen)
      rawA <- gen
    } yield {
      @tailrec
      def loop(a: A): (A, List[A]) =
        if (list.contains(a)) loop(munge(a))
        else (a, list)

      loop(rawA)
    }
}