package peschke.mock4s.predicates

import cats.Eq
import cats.data.{Chain, NonEmptyChain}
import cats.syntax.all._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}
import peschke.mock4s.models.JsonPath
import peschke.mock4s.models.JsonPath.Segment
import peschke.mock4s.predicates.Predicate.{Fixed, UsingCombinators, UsingEq}
import peschke.mock4s.utils.Circe._

object JsonPredicate extends PredicateWrapper[Json] {
  final case class WithPath(pathOpt: Option[JsonPath],
                            when: Fixed[Json] |+| UsingEq[Json]) extends Predicate[Json] {
    override def test(a: Json): Boolean =
      pathOpt.fold(when.test(a)) { path =>
        val expectingArray = path.segments.exists {
          case Segment.DownArray => true
          case _ => false
        }
        val pathResultsOpt: Option[Either[NonEmptyChain[Json], Json]] =
          NonEmptyChain
            .fromChain {
              path
                .segments
                .foldLeft[Chain[Json]](Chain.one(a))(_ <<: _)
            }
            .map { nel =>
              if (expectingArray) nel.asLeft
              else if (nel.tail.isEmpty) nel.head.asRight
              else Json.arr(nel.toNonEmptyVector.toVector: _*).asRight
            }

        // Special case: never accepts JSON that has nothing at the path
        if (when === WhenNever) pathResultsOpt.isEmpty
        else pathResultsOpt.exists {
          case Right(json) => when.test(json)
          case Left(jsonNec) => jsonNec.exists(when.test)
        }
      }
  }

  private val WhenNever: Fixed[Json] |+| UsingEq[Json] = lhs[Fixed[Json], UsingEq[Json]](Fixed.Never())

  type Base = WithPath

  override implicit val baseDecoder: Decoder[WithPath] = {
    val predDecoder = GeneratedDecoder[Fixed[Json] |+| UsingEq[Json]].decoder
    accumulatingDecoder[WithPath] { c =>
      (
        c.downField("path").asAcc[Option[JsonPath]],
        c.downField("when").asAcc(predDecoder)
      ).mapN(WithPath)
    }
  }
  override implicit val baseEncoder: Encoder[WithPath] = {
    val predEncoder = GeneratedEncoder[Fixed[Json] |+| UsingEq[Json]].encoder
    Encoder.instance[WithPath] { wp =>
      wp.pathOpt
        .filterNot(_.segments.isEmpty)
        .fold(Json.obj())(p => Json.obj("path" := p))
        .deepMerge(Json.obj("when" := predEncoder(wp.when)))
    }
  }

  implicit val baseEq: Eq[WithPath] = Eq.instance { (a,b) =>
    a.pathOpt === b.pathOpt
  }

  val always: Type = wrap {
    lhs[Base, UsingCombinators[Json, Base]](
      WithPath(none[JsonPath], lhs[Fixed[Json], UsingEq[Json]](Fixed.Always[Json]()))
    )
  }
  def always(path: JsonPath): Type = wrap {
    lhs[Base, UsingCombinators[Json, Base]](
      WithPath(path.some, lhs[Fixed[Json], UsingEq[Json]](Fixed.Always[Json]()))
    )
  }

  val never: Type = wrap {
    lhs[Base, UsingCombinators[Json, Base]](
      WithPath(none[JsonPath], lhs[Fixed[Json], UsingEq[Json]](Fixed.Never[Json]()))
    )
  }
  def never(path: JsonPath): Type = wrap {
    lhs[Base, UsingCombinators[Json, Base]](
      WithPath(path.some, lhs[Fixed[Json], UsingEq[Json]](Fixed.Never[Json]()))
    )
  }

  def is(sentinel: Json): Type = wrap {
    lhs[Base, UsingCombinators[Json, Base]](
      WithPath(none[JsonPath], rhs[Fixed[Json], UsingEq[Json]](UsingEq.Is[Json](sentinel)))
    )
  }

  def is(path: JsonPath, sentinel: Json): Type = wrap {
    lhs[Base, UsingCombinators[Json, Base]](
      WithPath(path.some, rhs[Fixed[Json], UsingEq[Json]](UsingEq.Is[Json](sentinel)))
    )
  }

  def in(sentinels: List[Json]): Type = wrap {
    lhs[Base, UsingCombinators[Json, Base]](
      WithPath(none[JsonPath], rhs[Fixed[Json], UsingEq[Json]](UsingEq.In[Json](sentinels)))
    )
  }

  def in(path: JsonPath, sentinels: List[Json]): Type = wrap {
    lhs[Base, UsingCombinators[Json, Base]](
      WithPath(path.some, rhs[Fixed[Json], UsingEq[Json]](UsingEq.In[Json](sentinels)))
    )
  }
}
