package peschke.mock4s

import cats.Eq
import cats.Show
import cats.data.Chain
import cats.data.NonEmptyChain
import cats.data.Validated
import cats.syntax.all._
import munit.Assertions
import munit.Location
import munit.internal.console.StackTraces
import munit.internal.difflib.ComparisonFailExceptionHandler
import munit.internal.difflib.Diffs
import org.scalacheck.Gen

trait MUnitCats { self: Assertions =>
  private def munitComparisonHandler
    (
        actualObtained: Any,
        actualExpected: Any
    )
    : ComparisonFailExceptionHandler =
    (message: String, _: String, _: String, loc: Location) =>
      failComparison(message, actualObtained, actualExpected)(loc)

  def assertValid[I: Show, V](validated: Validated[I, V])(implicit location: Location): V =
    validated.valueOr { i =>
      fail(s"Expected Valid(_), but was Invalid(${i.show}")
    }

  def assertInvalid[I, V: Show](validated: Validated[I, V])(implicit location: Location): I =
    validated.swap.valueOr { i =>
      fail(s"Expected Invalid(_), but was Valid(${i.show}")
    }

  def assertRight[L: Show, R](either: Either[L, R])(implicit location: Location): R =
    either.valueOr { i =>
      fail(s"Expected Right(_), but was Left(${i.show}")
    }

  def assertLeft[L, R: Show](either: Either[L, R])(implicit location: Location): L =
    either.swap.valueOr { i =>
      fail(s"Expected Left(_), but was Right(${i.show}")
    }

  def assertSome[A](option: Option[A])(implicit location: Location): A =
    option.getOrElse {
      fail("Expected Some(_), but was None")
    }

  @SuppressWarnings(Array("DisableSyntax.defaultArgs"))
  def assertEq[A: Eq: Show]
    (obtained:          A, expected: A, clue: Any = "values are not the same")
    (implicit location: Location)
    : Unit =
    StackTraces.dropInside {
      if (expected =!= obtained) {
        Diffs.assertNoDiff(
          obtained.show,
          expected.show,
          munitComparisonHandler(obtained, expected),
          munitPrint(clue),
          printObtainedAsStripMargin = false
        )(location)
        // try with `munitPrint` in case `.show` produces identical formatting for both values
        Diffs.assertNoDiff(
          munitPrint(obtained),
          munitPrint(expected),
          munitComparisonHandler(obtained, expected),
          munitPrint(clue),
          printObtainedAsStripMargin = false
        )(location)
        // try with `.toString` in case `munitPrint()` produces identical formatting for both values.
        Diffs.assertNoDiff(
          obtained.toString,
          expected.toString,
          munitComparisonHandler(obtained, expected),
          munitPrint(clue),
          printObtainedAsStripMargin = false
        )(location)
        failComparison(
          s"values are not equal even if they have the same `toString()`: $obtained",
          obtained,
          expected
        )(location)
      }
    }

  def chainsOf[A](gen: Gen[A]): Gen[Chain[A]] = Gen.listOf(gen).map(Chain.fromSeq)
  def nonEmptyChainsOf[A](gen: Gen[A]): Gen[NonEmptyChain[A]] = Gen.sized { size =>
    for {
      head <- gen
      tail <- Gen.resize(size, chainsOf(gen))
    } yield NonEmptyChain.fromChainPrepend(head, tail)
  }
}
object MUnitCats extends MUnitCats with Assertions
