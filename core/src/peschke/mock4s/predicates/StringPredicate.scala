package peschke.mock4s.predicates

import cats.Eq
import cats.data.{NonEmptyList, Validated}
import cats.syntax.all._
import io.circe.syntax._
import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import peschke.mock4s.algebras.PredicateChecker
import peschke.mock4s.models.|+|
import peschke.mock4s.models.|+|.syntax.LiftOps
import peschke.mock4s.utils.Circe._

import java.util.regex.PatternSyntaxException
import scala.util.matching.Regex

sealed trait StringTests {
  def upcast: StringTests = this
}

object StringTests {
  final case class StartsWith(prefix: String) extends StringTests

  object StartsWith {
    implicit val decoder: Decoder[StartsWith] =
      accumulatingDecoder(_.asAcc[String].map(StartsWith(_))).at("starts-with")

    implicit val encoder: Encoder[StartsWith] = Encoder.instance(sw => Json.obj("starts-with" := sw.prefix))
  }

  final case class EndsWith(suffix: String) extends StringTests

  object EndsWith {
    implicit val decoder: Decoder[EndsWith] =
      accumulatingDecoder(_.asAcc[String].map(EndsWith(_))).at("ends-with")

    implicit val encoder: Encoder[EndsWith] = Encoder.instance(ew => Json.obj("ends-with" := ew.suffix))
  }

  final case class Contains(substring: String) extends StringTests

  object Contains {
    implicit val decoder: Decoder[Contains] =
      accumulatingDecoder(_.asAcc[String].map(Contains(_))).at("contains")

    implicit val encoder: Encoder[Contains] = Encoder.instance(c => Json.obj("contains" := c.substring))
  }

  final case class Matches(regex: Regex) extends StringTests

  object Matches {
    implicit val decoder: Decoder[Matches] =
      accumulatingDecoder { c =>
        c.asAcc[String].andThen { raw =>
          Validated.catchOnly[PatternSyntaxException](raw.r).leftMap { pse =>
            DecodingFailure.fromThrowable(pse, c.history).pure[NonEmptyList]
          }
        }
      }.map(Matches(_)).at("matches")

    implicit val encoder: Encoder[Matches] = Encoder.instance(m => Json.obj("matches" := m.regex.pattern.pattern()))
  }

  implicit val decoder: Decoder[StringTests] = anyOf[StringTests](
    StartsWith.decoder.widen,
    EndsWith.decoder.widen,
    Contains.decoder.widen,
    Matches.decoder.widen
  )

  implicit val encoder: Encoder[StringTests] = Encoder.instance {
    case ssp@StartsWith(_) => ssp.asJson
    case ssp@EndsWith(_) => ssp.asJson
    case ssp@Contains(_) => ssp.asJson
    case ssp@Matches(_) => ssp.asJson
  }

  implicit val eq: Eq[StringTests] = Eq.instance {
    case (StartsWith(a), StartsWith(b)) => a === b
    case (EndsWith(a), EndsWith(b)) => a === b
    case (Contains(a), Contains(b)) => a === b
    case (Matches(a), Matches(b)) => a.regex === b.regex
    case _ => false
  }

  implicit val checker: PredicateChecker[String, StringTests] = (predicate, in) => predicate match {
    case StartsWith(prefix) => in.startsWith(prefix)
    case EndsWith(suffix) => in.endsWith(suffix)
    case Contains(substring) => in.contains(substring)
    case Matches(regex) => regex.matches(in)
  }
}

object StringPredicate extends PredicateWrapper[String, StringTests |+| Fixed[String] |+| UsingEq[String]] {
  val always: Type = wrap {
    Fixed
      .Always[String]().upcast
      .second[StringTests]
      .first[UsingEq[String]]
      .first[UsingCombinators[Base]]
  }

  val never: Type = wrap {
    Fixed.Never[String]()
      .upcast
      .second[StringTests]
      .first[UsingEq[String]]
      .first[UsingCombinators[Base]]
  }

  def is(sentinel: String): Type = wrap {
    UsingEq.Is(sentinel)
      .upcast
      .second[StringTests |+| Fixed[String]]
      .first[UsingCombinators[Base]]
  }


  def in(sentinels: List[String]): Type = wrap {
    UsingEq.In(sentinels)
      .upcast
      .second[StringTests |+| Fixed[String]]
      .first[UsingCombinators[Base]]
  }


  def startsWith(prefix: String): Type = wrap {
    StringTests.StartsWith(prefix)
      .upcast
      .first[Fixed[String]]
      .first[UsingEq[String]]
      .first[UsingCombinators[Base]]
  }


  def endsWith(suffix: String): Type = wrap {
    StringTests.EndsWith(suffix)
      .upcast
      .first[Fixed[String]]
      .first[UsingEq[String]]
      .first[UsingCombinators[Base]]
  }


  def contains(substring: String): Type = wrap {
    StringTests.Contains(substring)
      .upcast
      .first[Fixed[String]]
      .first[UsingEq[String]]
      .first[UsingCombinators[Base]]
  }


  def matches(regex: Regex): Type = wrap {
    StringTests.Matches(regex)
      .upcast
      .first[Fixed[String]]
      .first[UsingEq[String]]
      .first[UsingCombinators[Base]]
  }

}
