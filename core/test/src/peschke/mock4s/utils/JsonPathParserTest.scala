package peschke.mock4s.utils

import cats.data.Chain
import cats.data.Validated
import cats.syntax.all._
import munit.Assertions
import munit.Location
import munit.ScalaCheckSuite
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import peschke.mock4s.MUnitCats
import peschke.mock4s.models.JsonPath
import peschke.mock4s.models.JsonPath.Segment
import peschke.mock4s.models.JsonPath.Segment.AtIndex
import peschke.mock4s.models.JsonPath.Segment.BareField
import peschke.mock4s.models.JsonPath.Segment.DownArray
import peschke.mock4s.models.JsonPath.Segment.QuotedField
import peschke.mock4s.utils.JsonPathParserTest.TestInput
import peschke.mock4s.utils.JsonPathParserTest.atIndexes
import peschke.mock4s.utils.JsonPathParserTest.bareFields
import peschke.mock4s.utils.JsonPathParserTest.downArrays
import peschke.mock4s.utils.JsonPathParserTest.quotedFields

class JsonPathParserTest extends ScalaCheckSuite with MUnitCats {

  override val scalaCheckInitialSeed = "TzBo0TbyifUqctzySl5qSmlufMjegKiMXwomwl3ModG="

  test("parse empty string") {
    TestInput("", Chain.empty).run()
  }

  test("parse <$>") {
    TestInput("$", Chain.empty).run()
  }

  test("parse <$.[]>") {
    TestInput("$.[]", Chain.one(DownArray)).run()
  }

  test("parse <$[]>") {
    TestInput("$[]", Chain.one(DownArray)).run()
  }

  property("parse bare field selections") {
    val gen = Gen.resize(20, Gen.nonEmptyListOf(bareFields)).map(Chain.fromSeq).map { segmentsAndRaws =>
      val (rawStrings, segments) = segmentsAndRaws.unzip
      TestInput(rawStrings.mkString_("$.", ".", ""), segments)
    }
    forAll(gen)(_.run())
  }

  property("parse indexing into arrays") {
    val gen = Gen.resize(20, Gen.nonEmptyListOf(atIndexes)).map(Chain.fromSeq).map { segmentsAndRaws =>
      val (rawStrings, segments) = segmentsAndRaws.unzip
      TestInput(rawStrings.mkString_("$.", ".", ""), segments)
    }
    forAll(gen)(_.run())
  }

  property("parse quoted field selections") {
    val gen = Gen.resize(20, Gen.nonEmptyListOf(quotedFields)).map(Chain.fromSeq).map { segmentsAndRaws =>
      val (rawStrings, segments) = segmentsAndRaws.unzip
      TestInput(rawStrings.mkString_("$.", ".", ""), segments)
    }
    forAll(gen)(_.run())
  }

  property("parse chained selections") {
    val chainableSegments = Gen.oneOf(quotedFields, atIndexes, downArrays)
    val gen = for {
      base    <- Gen.oneOf[(String, Segment)](bareFields, quotedFields, atIndexes, downArrays)
      chained <- Gen.resize(20, Gen.nonEmptyListOf(chainableSegments)).map(Chain.fromSeq)
    } yield {
      val (baseRaw, baseSegment) = base
      val (chainedRaw, chainedSegments) = chained.unzip
      TestInput(
        s"$$.$baseRaw${chainedRaw.mkString_("")}",
        baseSegment +: chainedSegments
      )
    }
    forAll(gen)(_.run())
  }

  property("parse the output of JsonPath#render") {
    val segments = Gen.oneOf(
      quotedFields.map(_._2),
      bareFields.map(_._2),
      atIndexes.map(_._2),
      downArrays.map(_._2)
    )
    val gen = Gen.listOf(segments).map(s => JsonPath.fromChain(Chain.fromSeq(s))).map { jsonPath =>
      TestInput(jsonPath.render, jsonPath.segments)
    }
    forAll(gen)(_.run())
  }
}
object JsonPathParserTest extends Assertions with MUnitCats {
  final case class TestInput(raw: String, path: Chain[Segment]) {
    val expected: JsonPath = JsonPath(path)

    def run()(implicit loc: Location): Unit = {
      JsonPathParser.parse(raw) match {
        case Validated.Invalid(e)    =>
          fail(show"Unable to parse <$raw>:${e.mkString_("\n  ", "\n  ", "\n")}")
        case Validated.Valid(actual) =>
          assertEq(actual, expected)(implicitly, implicitly, loc)
      }
    }
  }

  def charsExcept(p: Char => Boolean): Gen[Char] = Gen.recursive[Char] { tryAgain =>
    arbitrary[Char].flatMap { c =>
      if (p(c)) tryAgain
      else Gen.const(c)
    }
  }

  val bareFields: Gen[(String, BareField)] = {
    val validChars = Gen.oneOf(Gen.alphaNumChar, Gen.oneOf('-', '_'))
    Gen
      .chooseNum(1, 20)
      .flatMap(Gen.stringOfN(_, validChars))
      .map(raw => (raw, BareField(raw)))
  }

  val quotedFields: Gen[(String, QuotedField)] = {
    val unescapedChar = charsExcept(c => c.isControl || c === '\\').map(c => s"$c" -> c)
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
    Gen
      .chooseNum(1, 20)
      .flatMap(Gen.listOfN(_, validChars))
      .map { chars =>
        val (encoded, raw) = chars.unzip
        (s"""["${encoded.mkString}"]""", QuotedField(raw.mkString))
      }
  }

  val atIndexes: Gen[(String, AtIndex)] =
    Gen.chooseNum(0, 9999).map { index =>
      (s"[$index]", AtIndex(index))
    }

  val downArrays: Gen[(String, DownArray.type)] = Gen.const("[]" -> DownArray)
}
