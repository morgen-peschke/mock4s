package peschke.mock4s.predicates

import cats.Eq
import cats.data.Chain
import cats.data.NonEmptyChain
import cats.syntax.all._
import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax._
import peschke.mock4s.algebras.PredicateChecker
import peschke.mock4s.algebras.PredicateChecker.syntax._
import peschke.mock4s.models.JsonPath
import peschke.mock4s.models.JsonPath.Segment
import peschke.mock4s.models.|+|
import peschke.mock4s.models.|+|.syntax._
import peschke.mock4s.utils.Circe._

sealed trait JsonTest {
  def upcast: JsonTest = this
}
object JsonTest       {
  final case class IsString(p: StringPredicate.Type)             extends JsonTest
  final case class IsNumber(p: BigDecimalPredicate.Type)         extends JsonTest
  final case class AtPath(path: JsonPath, p: JsonPredicate.Type) extends JsonTest

  implicit val decoder: Decoder[JsonTest] = anyOf[JsonTest](
    Decoder[StringPredicate.Type].at("string").map(IsString),
    Decoder[BigDecimalPredicate.Type].at("number").map(IsNumber),
    accumulatingDecoder { c =>
      (
        c.downField("at").asAcc[JsonPath],
        c.downField("when").asAcc[JsonPredicate.Type]
      ).mapN(AtPath)
    }
  )

  implicit val encoder: Encoder[JsonTest] = Encoder.instance {
    case IsString(p)     => Json.obj("string" := p)
    case IsNumber(p)     => Json.obj("number" := p)
    case AtPath(path, p) => Json.obj("at" := path, "when" := p)
  }

  implicit val eq: Eq[JsonTest] = Eq.instance {
    case (IsString(a), IsString(b))                   => a === b
    case (IsNumber(a), IsNumber(b))                   => a === b
    case (AtPath(pathA, predA), AtPath(pathB, predB)) => pathA === pathB && predA === predB
    case _                                            => false
  }

  implicit val checker: PredicateChecker[Json, JsonTest] = (predicate, in) =>
    predicate match {
      case IsString(p)     => in.asString.exists(_.satisfies(p))
      case IsNumber(p)     =>
        in.asNumber.exists(n => n.toBigDecimal.getOrElse(BigDecimal.valueOf(n.toDouble)).satisfies(p))
      case AtPath(path, p) =>
        val expectingArray = path.segments.exists {
          case Segment.DownArray => true
          case _                 => false
        }
        val pathResultsOpt: Option[Either[NonEmptyChain[Json], Json]] =
          NonEmptyChain
            .fromChain {
              path
                .segments
                .foldLeft[Chain[Json]](Chain.one(in))(_ <<: _)
            }
            .map { nel =>
              if (expectingArray) nel.asLeft
              else if (nel.tail.isEmpty) nel.head.asRight
              else Json.arr(nel.toNonEmptyVector.toVector: _*).asRight
            }

        pathResultsOpt.exists {
          case Right(jsonResult)   => jsonResult.satisfies(p)
          case Left(jsonResultNec) => jsonResultNec.exists(p.satisfiedBy(_))
        }
    }
}

object JsonPredicate extends PredicateWrapper[Json, Fixed[Json] |+| UsingEq[Json] |+| JsonTest] {
  val always: Type = wrap {
    Fixed.Always[Json]().upcast.first[UsingEq[Json]].first[JsonTest].first[UsingCombinators[Base]]
  }

  val never: Type = wrap {
    Fixed.Never[Json]().upcast.first[UsingEq[Json]].first[JsonTest].first[UsingCombinators[Base]]
  }

  def is(sentinel: Json): Type = wrap {
    UsingEq.Is[Json](sentinel).upcast.second[Fixed[Json]].first[JsonTest].first[UsingCombinators[Base]]
  }

  def in(sentinels: List[Json]): Type = wrap {
    UsingEq.In[Json](sentinels).upcast.second[Fixed[Json]].first[JsonTest].first[UsingCombinators[Base]]
  }

  def string(p: StringPredicate.Type): Type = wrap {
    JsonTest.IsString(p).upcast.second[Fixed[Json] |+| UsingEq[Json]].first[UsingCombinators[Base]]
  }

  def number(p: BigDecimalPredicate.Type): Type = wrap {
    JsonTest.IsNumber(p).upcast.second[Fixed[Json] |+| UsingEq[Json]].first[UsingCombinators[Base]]
  }

  def atPath(path: JsonPath, andThen: Type): Type = wrap {
    JsonTest.AtPath(path, andThen).upcast.second[Fixed[Json] |+| UsingEq[Json]].first[UsingCombinators[Base]]
  }
}
