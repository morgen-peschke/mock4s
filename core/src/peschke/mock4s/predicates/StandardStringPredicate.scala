package peschke.mock4s.predicates

import cats.syntax.all._
import cats.data.{NonEmptyList, Validated}
import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import io.circe.syntax._

import scala.util.matching.Regex
import peschke.mock4s.utils.Circe._

import java.util.regex.PatternSyntaxException

sealed abstract class StandardStringPredicate(pred: String => Boolean) extends Predicate[String] {
  override def test(a: String): Boolean = pred(a)
}
object StandardStringPredicate {
  final case class StartsWith(prefix: String) extends StandardStringPredicate(_.startsWith(prefix))
  object StartsWith {
    implicit val decoder: Decoder[StartsWith] =
      accumulatingDecoder(_.asAcc[String].map(StartsWith(_))).at("starts-with")

    implicit val encoder: Encoder[StartsWith] = Encoder.instance(sw => Json.obj("starts-with" := sw.prefix))
  }

  final case class EndsWith(suffix: String) extends StandardStringPredicate(_.endsWith(suffix))
  object EndsWith {
    implicit val decoder: Decoder[EndsWith] =
      accumulatingDecoder(_.asAcc[String].map(EndsWith(_))).at("ends-with")

    implicit val encoder: Encoder[EndsWith] = Encoder.instance(ew => Json.obj("ends-with" := ew.suffix))
  }

  final case class Contains(substring: String) extends StandardStringPredicate(_.contains(substring))
  object Contains {
    implicit val decoder: Decoder[Contains] =
      accumulatingDecoder(_.asAcc[String].map(Contains(_))).at("contains")

    implicit val encoder: Encoder[Contains] = Encoder.instance(c => Json.obj("contains" := c.substring))
  }

  final case class Matches(regex: Regex) extends StandardStringPredicate(regex.matches(_))
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

  implicit val decoder: Decoder[StandardStringPredicate] = anyOf[StandardStringPredicate](
    StartsWith.decoder.widen,
    EndsWith.decoder.widen,
    Contains.decoder.widen,
    Matches.decoder.widen
  )

  implicit val encoder: Encoder[StandardStringPredicate] = Encoder.instance {
    case ssp @ StartsWith(_) => ssp.asJson
    case ssp @ EndsWith(_) => ssp.asJson
    case ssp @ Contains(_) => ssp.asJson
    case ssp @ Matches(_) => ssp.asJson
  }
}