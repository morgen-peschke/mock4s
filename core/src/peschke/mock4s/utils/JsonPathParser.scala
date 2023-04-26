package peschke.mock4s.utils

import cats.data.Chain
import cats.data.NonEmptyChain
import cats.data.Validated
import cats.data.ValidatedNec
import cats.syntax.all._
import peschke.mock4s.algebras.Parser
import peschke.mock4s.algebras.Parser.ParseError
import peschke.mock4s.algebras.Parser.Result
import peschke.mock4s.algebras.Parser.State
import peschke.mock4s.models.JsonPath
import peschke.mock4s.models.JsonPath.Segment
import peschke.mock4s.models.JsonPath.Segment.BareField
import peschke.mock4s.models.JsonPath.Segment.QuotedField

object JsonPathParser {
  object IdentifierChar extends supertagged.NewType[Char]

  type IdentifierChar = IdentifierChar.Type

  object HexChar extends supertagged.NewType[Char] {
    def isHexDigit(c: Char): Boolean =
      c.isDigit || c === 'A' || c === 'B' || c === 'C' || c === 'D' || c === 'E' || c === 'F'

    val parser: Parser[Type] = Parser.namedAccumulating("hexCharParser") { input =>
      input.uncons match {
        case Some(c -> state) if isHexDigit(c.toUpper) => Result.valid(apply(c), state)
        case Some(c -> _)                              =>
          ParseError.one(s"Expected hex digit, but was '$c'", input).invalid
        case None                                      =>
          ParseError.one("Expected hex digit, but ran out of input", input).invalid
      }
    }
  }

  object bareIdentifier {
    val validChar: Parser[Option[IdentifierChar]] =
      Parser.accumulating[Option[IdentifierChar]] { input =>
        input.uncons match {
          case None                => Result.valid(none[IdentifierChar], input)
          case Some(char -> state) =>
            char match {
              case '.' | '['                                        => Result.valid(none[IdentifierChar], input)
              case c if c.isLetterOrDigit || c === '-' || c === '_' => Result.valid(IdentifierChar(char).some, state)
              case _                                                =>
                ParseError.one("Base JSON field names can only contain letters, digits, '-', or '_'", state).invalid
            }
        }
      }

    val bareFieldParser: Parser[Segment] = validChar.repeatWhileSomeNec.map { fieldChars =>
      BareField(fieldChars.map(IdentifierChar.raw).mkString_(""))
    }
  }

  object quotedIdentifier {
    sealed trait Escape

    case object Escape extends Escape

    val unescaped: Parser[Option[Either[Escape, IdentifierChar]]] =
      Parser.accumulating[Option[Either[Escape, IdentifierChar]]] { input =>
        input.uncons match {
          case None                => Result.valid(none[Either[Escape, IdentifierChar]], input)
          case Some(char -> state) =>
            char match {
              case '\\' => Result.valid[Option[Either[Escape, IdentifierChar]]](Escape.asLeft.some, state)
              case '"'  => Result.valid(none[Either[Escape, IdentifierChar]], input)
              case _    =>
                Result.valid[Option[Either[Escape, IdentifierChar]]](IdentifierChar(char).asRight.some, state)
            }
        }
      }

    val fourHexDigits: Parser[String] =
      Parser.instance[String] { s0 =>
        for {
          Result(c0, s1) <- HexChar.parser.parseE(s0)
          Result(c1, s2) <- HexChar.parser.parseE(s1)
          Result(c2, s3) <- HexChar.parser.parseE(s2)
          Result(c3, s4) <- HexChar.parser.parseE(s3)
        } yield Result(s"$c0$c1$c2$c3", s4)
      }

    val escaped: Parser[IdentifierChar] =
      Parser.accumulating[IdentifierChar] { input =>
        input.uncons match {
          case None                => ParseError.one("expected at least one character after '\'", input).invalid
          case Some(char -> state) =>
            def validChar(c: Char): ParseError.OrValid[IdentifierChar] = Result.valid(IdentifierChar(c), state)
            char match {
              case '\\' | '/' | '"' => validChar(char)
              case 'b'              => validChar('\u0008')
              case 'f'              => validChar('\u000C')
              case 'n'              => validChar('\n')
              case 't'              => validChar('\t')
              case 'u'              =>
                fourHexDigits
                  .parseV(state)
                  .andThen { hexString =>
                    Validated
                      .catchOnly[NumberFormatException](Integer.parseInt(hexString.value, 16).toChar)
                      .leftMap { _ =>
                        ParseError.one("expected 4 hex digits after \\u", state)
                      }
                      .map(c => hexString.as(IdentifierChar(c)))
                  }
              case _                =>
                ParseError.one(s"invalid escape '\\$char'", input).invalid
            }
        }
      }

    val validChar: Parser[Option[IdentifierChar]] =
      Parser.accumulating[Option[IdentifierChar]] { input =>
        unescaped.parseV(input).andThen { result =>
          result.value match {
            case None               => result.as(none[IdentifierChar]).valid
            case Some(Right(char))  => result.as(char.some).valid
            case Some(Left(Escape)) => escaped.parseV(result.state).map(_.map(_.some))
          }
        }
      }

    val quotedSegmentParser: Parser[Segment] = validChar.repeatWhileSomeNec.map { fieldChars =>
      QuotedField(fieldChars.map(IdentifierChar.raw).mkString_(""))
    }
  }

  val arrayIndex: Parser[Segment] = Parser.accumulating[Segment] { input =>
    val digits = input.value.takeWhile(_.isDigit)
    val currentIndex = input.index + digits.length.toInt
    val leftover = input.value.dropWhile(_.isDigit)
    val state = State(leftover, currentIndex, input.history)
    Validated
      .fromOption(
        digits.mkString_("").toIntOption,
        ParseError.one("JSON array index is not an integer", input)
      )
      .map(Segment.AtIndex)
      .map(Result(_, state))
  }

  object jsonSegment {
    val period: Parser[Char] = Parser.fixed('.')
    val openBrace: Parser[Char] = Parser.fixed('[')
    val closeBrace: Parser[Char] = Parser.fixed(']')
    val startOfQuotedField: Parser[String] = Parser.fixed("[\"")
    val endOfQuotedField: Parser[String] = Parser.fixed("\"]")

    val arrayBraces: Parser[Segment] = Parser.fixed("[]").as[Segment](Segment.DownArray).withEnclosingName
    val indexBraces: Parser[Segment] = ((openBrace *> arrayIndex) <* closeBrace).withEnclosingName

    val downArray: Parser[Segment] = Parser.findValid(arrayBraces, indexBraces)

    val quotedField: Parser[Segment] =
      (startOfQuotedField *> quotedIdentifier.quotedSegmentParser) <* endOfQuotedField

    val chainableSegments: Parser[Chain[Segment]] =
      Parser.findValid[Segment](quotedField, downArray).repeated.withEnclosingName

    val fieldAndChainedSegments: Parser[NonEmptyChain[Segment]] =
      (Parser.findValid(quotedField, bareIdentifier.bareFieldParser) ~+:> chainableSegments).withEnclosingName

    val arrayAndChainedSegments: Parser[NonEmptyChain[Segment]] =
      (downArray ~+:> chainableSegments).withEnclosingName

    val chunk: Parser[NonEmptyChain[Segment]] =
      period *>
        Parser.findValid(arrayAndChainedSegments, fieldAndChainedSegments).withEnclosingName

    val segmentParser: Parser[NonEmptyChain[Segment]] =
      chunk.repeatedNec.map(_.flatten).withEnclosingName
  }

  val parser: Parser[Chain[Segment]] =
    Parser.findValid(
      Parser.End.as(Chain.empty[Segment]).withName("empty path"),
      (Parser.fixed('.') *> Parser.End).as(Chain.empty[Segment]).withName("root path"),
      (jsonSegment.segmentParser <* Parser.End).map(result => result.toChain)
    )

  def parse(raw: String): ValidatedNec[ParseError, JsonPath] =
    parser.parseV(raw).map { segments =>
      JsonPath(segments.value)
    }
}
