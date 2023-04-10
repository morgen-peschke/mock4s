package peschke.mock4s

import cats.Eq
import cats.syntax.all._
import cats.data.Validated
import io.circe.{Decoder, Encoder, Json, Printer}
import munit._
import munit.internal.console.StackTraces
import munit.internal.difflib.{ComparisonFailExceptionHandler, Diffs}
import peschke.mock4s.utils.Circe._

import scala.annotation.nowarn

trait MUnitCirce { self: Assertions =>

  private def munitComparisonHandler(
                                      actualObtained: Any,
                                      actualExpected: Any
                                    ): ComparisonFailExceptionHandler =
    (message: String, _: String, _: String, loc: Location) => failComparison(message, actualObtained, actualExpected)(loc)

  def assertDecodes[A: Eq : Decoder](json: Json, expected: A, clue: Any = "decoded values are not the same")(implicit location: Location): Unit =
    StackTraces.dropInside {
      json.hcursor.asAcc[A] match {
        case Validated.Invalid(e) =>
          fail(e.mkString_(s"Json did not decode successfully:\n  ", "\n  ", "\n"))(location)
        case Validated.Valid(obtained) =>
          if (expected =!= obtained) {
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
    }

  // Macro shenanigans means the compiler thinks `location` isn't used
  @nowarn("cat=unused")
  def assertEncodes[A: Eq : Encoder](value: A, expected: Json, clue: Any = "encoded json are not the same")(implicit location: Location): Unit =
    StackTraces.dropInside {
      val obtained = Encoder[A].apply(value)
      if (expected =!= obtained) {
        Diffs.assertNoDiff(
          obtained.printWith(Printer.spaces2SortKeys),
          expected.printWith(Printer.spaces2SortKeys),
          munitComparisonHandler(obtained, expected),
          munitPrint(clue),
          printObtainedAsStripMargin = false
        )(location)
        // try with `munitPrint()` in case JSON serialization produces identical formatting for both values.
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
          s"values are not equal even if they have the same `toString()` and serialization: $obtained",
          obtained,
          expected
        )(location)
      }
    }

  def assertCodec[A: Eq : Decoder : Encoder](value: A, json: Json)(implicit location: Location): Unit = {
    assertEncodes(value, json)(Eq[A], Encoder[A], location)
    assertDecodes(json, value)(Eq[A], Decoder[A], location)
  }
}
