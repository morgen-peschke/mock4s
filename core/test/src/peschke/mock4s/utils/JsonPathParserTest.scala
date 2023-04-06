package peschke.mock4s.utils

import cats.data.Validated
import cats.syntax.all._
import munit.{Assertions, Location, ScalaCheckSuite}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import peschke.mock4s.models.JsonPath
import peschke.mock4s.models.JsonPath.Segment
import peschke.mock4s.models.JsonPath.Segment.{AtIndex, DownArray, DownField}
import peschke.mock4s.utils.JsonPathParserTest.{TestInput, atIndexSegments, bareFields, downArraySegments, quotedFields}

class JsonPathParserTest extends ScalaCheckSuite {

  override val scalaCheckInitialSeed = "TzBo0TbyifUqctzySl5qSmlufMjegKiMXwomwl3ModG="

  test("parse empty string") {
    TestInput("", Nil).run()
  }

  test("parse <.>") {
    TestInput(".", Nil).run()
  }

  test("parse <.[]>") {
    TestInput(".[]", DownArray :: Nil).run()
  }

  property("parse bare field selections") {
    val gen = Gen.resize(20, Gen.nonEmptyListOf(bareFields)).map { segmentsAndRaws =>
      val (rawStrings, segments) = segmentsAndRaws.unzip
      TestInput(rawStrings.mkString(".", ".", ""), segments)
    }
    forAll(gen)(_.run())
  }

  property("parse indexing into arrays") {
    val gen = Gen.resize(20, Gen.nonEmptyListOf(atIndexSegments)).map { segmentsAndRaws =>
      val (rawStrings, segments) = segmentsAndRaws.unzip
      TestInput(rawStrings.mkString(".", ".", ""), segments)
    }
    forAll(gen)(_.run())
  }

  property("parse quoted field selections") {
    val gen = Gen.resize(20, Gen.nonEmptyListOf(quotedFields)).map { segmentsAndRaws =>
      val (rawStrings, segments) = segmentsAndRaws.unzip
      TestInput(rawStrings.mkString(".", ".", ""), segments)
    }
    forAll(gen)(_.run())
  }

  property("parse chained selections") {
    val chainableSegments = Gen.oneOf(quotedFields, atIndexSegments, downArraySegments)
    val gen = for {
      base <- Gen.oneOf[(String, Segment)](bareFields, quotedFields, atIndexSegments, downArraySegments)
      chained <- Gen.resize(20, Gen.nonEmptyListOf(chainableSegments))
    } yield {
      val (baseRaw, baseSegment) = base
      val (chainedRaw, chainedSegments) = chained.unzip
      TestInput(
        s".$baseRaw${chainedRaw.mkString}",
        baseSegment :: chainedSegments
      )
    }
    forAll(gen)(_.run())
  }
}
object JsonPathParserTest extends Assertions {
  final case class TestInput(raw: String, path: List[Segment]) {
    val expected: JsonPath = JsonPath(path, raw)

    def run()(implicit loc: Location): Unit = {
      JsonPathParser.parse(raw) match {
        case Validated.Invalid(e) =>
          fail(show"Unable to parse <$raw>:${e.mkString_("\n  ", "\n  ", "\n")}")
        case Validated.Valid(actual) =>
          assertEquals(actual, expected)(loc, implicitly)
      }
    }
  }

  def charsExcept(p: Char => Boolean): Gen[Char] = Gen.recursive[Char] { tryAgain =>
    arbitrary[Char].flatMap { c =>
      if (p(c)) tryAgain
      else Gen.const(c)
    }
  }

  val bareFields: Gen[(String, DownField)] = {
    val forbiddenChars = "'\"[].".toSet
    val validChars = charsExcept(c => c.isControl || forbiddenChars(c))
    Gen
      .chooseNum(1, 20)
      .flatMap(Gen.stringOfN(_, validChars))
      .map(raw => (raw, Segment.DownField(raw)))
  }

  val quotedFields: Gen[(String, DownField)] = {
    val unescapedChar = charsExcept(c => c.isControl || c == '\\').map(c => s"$c" -> c)
    val escapedCharacter = Gen.oneOf(
      Gen.const("\\\\" -> '\\'),
      Gen.const("\\/" -> '/'),
      Gen.const("\\\"" -> '"'),
      Gen.const("\\b" -> '\u0008'),
      Gen.const("\\f" -> '\u000C'),
      Gen.const("\\n" -> '\n'),
      Gen.const("\\t" -> '\t'),
      arbitrary[Char].map { c =>
        f"\\u${c.toInt}%04x" -> c
      }
    )
    val validChars = Gen.oneOf(escapedCharacter, unescapedChar)
    Gen.chooseNum(1, 20)
      .flatMap(Gen.listOfN(_, validChars))
      .map { chars =>
        val (encoded, raw) = chars.unzip
        (s"""["${encoded.mkString}"]""", Segment.DownField(raw.mkString))
      }
  }

  val atIndexSegments: Gen[(String, AtIndex)] =
   Gen.chooseNum(0, 9999).map { index =>
     (s"[$index]", AtIndex(index))
   }

  val downArraySegments: Gen[(String, DownArray.type)] = Gen.const("[]" -> DownArray)
}
