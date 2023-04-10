package peschke.mock4s.predicates

import cats.data.{Chain, NonEmptyChain}
import cats.syntax.all._
import cats.{Defer, Eq, PartialOrder}
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json, JsonObject}
import peschke.mock4s.models.JsonPath
import peschke.mock4s.models.JsonPath.Segment
import peschke.mock4s.predicates.Predicate.{Fixed, UsingCombinators, UsingEq, UsingOrder}
import peschke.mock4s.utils.Circe._
import peschke.mock4s.utils.Orphans._

import scala.annotation.nowarn

object JsonPredicate extends PredicateWrapper[Json] {
  final case class WithPath(pathOpt: Option[JsonPath], when: Fixed[Json] |+| UsingEq[Json] |+| UsingOrder[Json])
      extends Predicate[Json] {
    override def test(a: Json): Boolean =
      pathOpt.fold(when.test(a)) { path =>
        val expectingArray = path.segments.exists {
          case Segment.DownArray => true
          case _                 => false
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
        else
          pathResultsOpt.exists {
            case Right(json)   => when.test(json)
            case Left(jsonNec) => jsonNec.exists(when.test)
          }
      }
  }

  private val WhenNever: Fixed[Json] |+| UsingEq[Json] |+| UsingOrder[Json] =
    lhs[Fixed[Json] |+| UsingEq[Json], UsingOrder[Json]](
      lhs[Fixed[Json], UsingEq[Json]](Fixed.Never())
    )

  type Base = WithPath

  implicit val jsonOrder: PartialOrder[Json] =
    Defer[PartialOrder].fix[Json] { recurse =>
      val booleanPA = PartialOrder[Boolean]
      val bigDecPA = PartialOrder[BigDecimal]
      val stringPA = PartialOrder[String]
      val vectorPA = cats.instances.vector.catsKernelStdPartialOrderForVector(recurse)
      val fieldsPA = cats
        .instances.vector.catsKernelStdPartialOrderForVector(
          cats.instances.tuple.catsKernelStdPartialOrderForTuple2(stringPA, recurse)
        )
      val objectPA = PartialOrder.from[JsonObject] { (a, b) =>
        fieldsPA.partialCompare(a.toVector.sortBy(_._1), b.toVector.sortBy(_._1))
      }

      @nowarn
      def notComparable[A](a: A): Double = Double.NaN

      PartialOrder.from { (lhs, rhs) =>
        lhs.fold(
          jsonNull = lhs.fold(
            jsonNull = 0.0,
            jsonBoolean = notComparable(_),
            jsonNumber = notComparable(_),
            jsonString = notComparable(_),
            jsonArray = notComparable(_),
            jsonObject = notComparable(_)
          ),
          jsonBoolean = a =>
            rhs.fold(
              jsonNull = Double.NaN,
              jsonBoolean = b => booleanPA.partialCompare(a, b),
              jsonNumber = notComparable(_),
              jsonString = notComparable(_),
              jsonArray = notComparable(_),
              jsonObject = notComparable(_)
            ),
          jsonNumber = a =>
            rhs.fold(
              jsonNull = Double.NaN,
              jsonBoolean = notComparable(_),
              jsonNumber = b =>
                bigDecPA.partialCompare(
                  a.toBigDecimal.getOrElse(BigDecimal.valueOf(a.toDouble)),
                  b.toBigDecimal.getOrElse(BigDecimal.valueOf(b.toDouble))
                ),
              jsonString = notComparable(_),
              jsonArray = notComparable(_),
              jsonObject = notComparable(_)
            ),
          jsonString = a =>
            rhs.fold(
              jsonNull = Double.NaN,
              jsonBoolean = notComparable(_),
              jsonNumber = notComparable(_),
              jsonString = b => stringPA.partialCompare(a, b),
              jsonArray = notComparable(_),
              jsonObject = notComparable(_)
            ),
          jsonArray = a =>
            rhs.fold(
              jsonNull = Double.NaN,
              jsonBoolean = notComparable(_),
              jsonNumber = notComparable(_),
              jsonString = notComparable(_),
              jsonArray = b => vectorPA.partialCompare(a, b),
              jsonObject = notComparable(_)
            ),
          jsonObject = a =>
            rhs.fold(
              jsonNull = Double.NaN,
              jsonBoolean = notComparable(_),
              jsonNumber = notComparable(_),
              jsonString = notComparable(_),
              jsonArray = notComparable(_),
              jsonObject = b => objectPA.partialCompare(a, b)
            )
        )
      }
    }

  override implicit val baseDecoder: Decoder[WithPath] = {
    val predDecoder = GeneratedDecoder[Fixed[Json] |+| UsingEq[Json] |+| UsingOrder[Json]].decoder
    accumulatingDecoder[WithPath] { c =>
      (
        c.downField("path").asAcc[Option[JsonPath]],
        c.downField("when").asAcc(predDecoder)
      ).mapN(WithPath)
    }
  }
  override implicit val baseEncoder: Encoder[WithPath] = {
    val predEncoder = GeneratedEncoder[Fixed[Json] |+| UsingEq[Json] |+| UsingOrder[Json]].encoder
    Encoder.instance[WithPath] { wp =>
      wp.pathOpt
        .filterNot(_.segments.isEmpty)
        .fold(Json.obj())(p => Json.obj("path" := p))
        .deepMerge(Json.obj("when" := predEncoder(wp.when)))
    }
  }

  implicit val baseEq: Eq[WithPath] = Eq.instance { (a, b) =>
    a.pathOpt === b.pathOpt
  }

  val always: Type = wrap {
    lhs[Base, UsingCombinators[Json, Base]](
      WithPath(
        none[JsonPath],
        lhs[Fixed[Json] |+| UsingEq[Json], UsingOrder[Json]](lhs[Fixed[Json], UsingEq[Json]](Fixed.Always[Json]()))
      )
    )
  }
  def always(path: JsonPath): Type = wrap {
    lhs[Base, UsingCombinators[Json, Base]](
      WithPath(
        path.some,
        lhs[Fixed[Json] |+| UsingEq[Json], UsingOrder[Json]](
          lhs[Fixed[Json], UsingEq[Json]](Fixed.Always[Json]())
        )
      )
    )
  }

  val never: Type = wrap {
    lhs[Base, UsingCombinators[Json, Base]](
      WithPath(
        none[JsonPath],
        lhs[Fixed[Json] |+| UsingEq[Json], UsingOrder[Json]](lhs[Fixed[Json], UsingEq[Json]](Fixed.Never[Json]()))
      )
    )
  }
  def never(path: JsonPath): Type = wrap {
    lhs[Base, UsingCombinators[Json, Base]](
      WithPath(
        path.some,
        lhs[Fixed[Json] |+| UsingEq[Json], UsingOrder[Json]](lhs[Fixed[Json], UsingEq[Json]](Fixed.Never[Json]()))
      )
    )
  }

  def is(sentinel: Json): Type = wrap {
    lhs[Base, UsingCombinators[Json, Base]](
      WithPath(
        none[JsonPath],
        lhs[Fixed[Json] |+| UsingEq[Json], UsingOrder[Json]](rhs[Fixed[Json], UsingEq[Json]](UsingEq.Is[Json](sentinel)))
      )
    )
  }

  def is(path: JsonPath, sentinel: Json): Type = wrap {
    lhs[Base, UsingCombinators[Json, Base]](
      WithPath(
        path.some,
        lhs[Fixed[Json] |+| UsingEq[Json], UsingOrder[Json]](rhs[Fixed[Json], UsingEq[Json]](UsingEq.Is[Json](sentinel)))
      )
    )
  }

  def in(sentinels: List[Json]): Type = wrap {
    lhs[Base, UsingCombinators[Json, Base]](
      WithPath(
        none[JsonPath],
        lhs[Fixed[Json] |+| UsingEq[Json], UsingOrder[Json]](rhs[Fixed[Json], UsingEq[Json]](UsingEq.In[Json](sentinels)))
      )
    )
  }

  def in(path: JsonPath, sentinels: List[Json]): Type = wrap {
    lhs[Base, UsingCombinators[Json, Base]](
      WithPath(
        path.some,
        lhs[Fixed[Json] |+| UsingEq[Json], UsingOrder[Json]](rhs[Fixed[Json], UsingEq[Json]](UsingEq.In[Json](sentinels)))
      )
    )
  }

  def lessThan(sentinel: Json): Type = wrap {
    lhs[Base, UsingCombinators[Json, Base]](
      WithPath(
        none[JsonPath],
        rhs[Fixed[Json] |+| UsingEq[Json], UsingOrder[Json]](
          UsingOrder.LessThan[Json](sentinel)
        )
      )
    )
  }

  def lessThanEq(sentinel: Json): Type = wrap {
    lhs[Base, UsingCombinators[Json, Base]](
      WithPath(
        none[JsonPath],
        rhs[Fixed[Json] |+| UsingEq[Json], UsingOrder[Json]](
          UsingOrder.LessThanEq[Json](sentinel)
        )
      )
    )
  }

  def greaterThan(sentinel: Json): Type = wrap {
    lhs[Base, UsingCombinators[Json, Base]](
      WithPath(
        none[JsonPath],
        rhs[Fixed[Json] |+| UsingEq[Json], UsingOrder[Json]](
          UsingOrder.GreaterThan[Json](sentinel)
        )
      )
    )
  }

  def greaterThanEq(sentinel: Json): Type = wrap {
    lhs[Base, UsingCombinators[Json, Base]](
      WithPath(
        none[JsonPath],
        rhs[Fixed[Json] |+| UsingEq[Json], UsingOrder[Json]](
          UsingOrder.GreaterThanEq[Json](sentinel)
        )
      )
    )
  }
}
