package peschke.mock4s.predicates

import cats.Eq
import cats.data.{NonEmptyList, Validated}
import cats.syntax.all._
import io.circe.syntax._
import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import peschke.mock4s.predicates.Predicate.{Fixed, UsingCombinators, UsingEq}
import peschke.mock4s.utils.Circe._

import java.util.regex.PatternSyntaxException
import scala.util.matching.Regex

object StringPredicate extends PredicateWrapper[String] {
  sealed abstract class StringTests(pred: String => Boolean) extends Predicate[String] {
    override def test(a: String): Boolean = pred(a)
  }
  object StringTests {
    final case class StartsWith(prefix: String) extends StringTests(_.startsWith(prefix))

    object StartsWith {
      implicit val decoder: Decoder[StartsWith] =
        accumulatingDecoder(_.asAcc[String].map(StartsWith(_))).at("starts-with")

      implicit val encoder: Encoder[StartsWith] = Encoder.instance(sw => Json.obj("starts-with" := sw.prefix))
    }

    final case class EndsWith(suffix: String) extends StringTests(_.endsWith(suffix))

    object EndsWith {
      implicit val decoder: Decoder[EndsWith] =
        accumulatingDecoder(_.asAcc[String].map(EndsWith(_))).at("ends-with")

      implicit val encoder: Encoder[EndsWith] = Encoder.instance(ew => Json.obj("ends-with" := ew.suffix))
    }

    final case class Contains(substring: String) extends StringTests(_.contains(substring))

    object Contains {
      implicit val decoder: Decoder[Contains] =
        accumulatingDecoder(_.asAcc[String].map(Contains(_))).at("contains")

      implicit val encoder: Encoder[Contains] = Encoder.instance(c => Json.obj("contains" := c.substring))
    }

    final case class Matches(regex: Regex) extends StringTests(regex.matches(_))

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
  }

  type Base = StringTests |+| Fixed[String] |+| UsingEq[String]

  override implicit val baseDecoder: Decoder[Base] = GeneratedDecoder[Base].decoder
  override implicit val baseEncoder: Encoder[Base] = GeneratedEncoder[Base].encoder

  val always: Type = wrap {
    lhs[Base, UsingCombinators[String, Base]](
      lhs[StringTests |+| Fixed[String], UsingEq[String]](
        rhs[StringTests, Fixed[String]](Fixed.Always[String]())
      )
    )
  }

  val never: Type = wrap {
    lhs[Base, UsingCombinators[String, Base]](
      lhs[StringTests |+| Fixed[String], UsingEq[String]](
        rhs[StringTests, Fixed[String]](Fixed.Never[String]())
      )
    )
  }

  def is(sentinel: String): Type = wrap {
    lhs[Base, UsingCombinators[String, Base]](
      rhs[StringTests |+| Fixed[String], UsingEq[String]](UsingEq.Is[String](sentinel))
    )
  }

  def in(sentinels: List[String]): Type = wrap {
    lhs[Base, UsingCombinators[String, Base]](
      rhs[StringTests |+| Fixed[String], UsingEq[String]](UsingEq.In[String](sentinels))
    )
  }

  def startsWith(prefix: String): Type = wrap {
    lhs[Base, UsingCombinators[String, Base]](
      lhs[StringTests |+| Fixed[String], UsingEq[String]](
        lhs[StringTests, Fixed[String]](StringTests.StartsWith(prefix))
      )
    )
  }

  def endsWith(suffix: String): Type = wrap {
    lhs[Base, UsingCombinators[String, Base]](
      lhs[StringTests |+| Fixed[String], UsingEq[String]](
        lhs[StringTests, Fixed[String]](StringTests.EndsWith(suffix))
      )
    )
  }

  def contains(substring: String): Type = wrap {
    lhs[Base, UsingCombinators[String, Base]](
      lhs[StringTests |+| Fixed[String], UsingEq[String]](
        lhs[StringTests, Fixed[String]](StringTests.Contains(substring))
      )
    )
  }

  def matches(regex: Regex): Type = wrap {
    lhs[Base, UsingCombinators[String, Base]](
      lhs[StringTests |+| Fixed[String], UsingEq[String]](
        lhs[StringTests, Fixed[String]](StringTests.Matches(regex))
      )
    )
  }
}