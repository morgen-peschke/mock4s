package peschke.mock4s.predicates

import cats.Eq
import cats.data.{Chain, NonEmptyChain}
import cats.syntax.all._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}
import peschke.mock4s.algebras.PredicateChecker
import peschke.mock4s.algebras.PredicateChecker.syntax._
import peschke.mock4s.models.JsonPath.Segment
import peschke.mock4s.models.{JsonPath, |+|}
import peschke.mock4s.models.|+|.syntax._
import peschke.mock4s.utils.Circe._

final case class JsonTest(pathOpt: Option[JsonPath], when: Fixed[Json] |+| UsingEq[Json] |+| UsingOrder[Json])
object JsonTest {
  implicit val decoder: Decoder[JsonTest] = {
    val predDecoder = Decoder[Fixed[Json] |+| UsingEq[Json] |+| UsingOrder[Json]]
    accumulatingDecoder[JsonTest] { c =>
      if (c.keys.exists(_.exists(_ === "path")))
        (
          c.downField("path").asAcc[JsonPath].map(_.some),
          c.downField("when").asAcc(predDecoder)
        ).mapN(JsonTest(_, _))
      else c.asAcc(predDecoder).map(JsonTest(None, _))
    }
  }

  implicit val encoder: Encoder[JsonTest] = {
    val predEncoder = Encoder[Fixed[Json] |+| UsingEq[Json] |+| UsingOrder[Json]]
    Encoder.instance[JsonTest] { wp =>
      wp.pathOpt.fold(predEncoder(wp.when)) { path =>
        Json.obj("path" := path, "when" := predEncoder(wp.when))
      }
    }
  }

  // This has to be explicit or it'll return `null`. Probably initialization order issues
  private implicit val whenChecker: PredicateChecker[Json, Fixed[Json] |+| UsingEq[Json] |+| UsingOrder[Json]] =
    Fixed.predicateChecker[Json] |+| UsingEq.checker[Json] |+| UsingOrder.checker[Json]

  implicit lazy val checker: PredicateChecker[Json, JsonTest] = new PredicateChecker[Json, JsonTest] {
    private val WhenNever: Fixed[Json] |+| UsingEq[Json] |+| UsingOrder[Json] =
      Fixed.Never[Json]().upcast.first[UsingEq[Json]].first[UsingOrder[Json]]

    override def test(withPath: JsonTest, json: Json): Boolean =
      withPath.pathOpt.fold(json.satisfies(withPath.when)) { path =>
        val expectingArray = path.segments.exists {
          case Segment.DownArray => true
          case _ => false
        }
        val pathResultsOpt: Option[Either[NonEmptyChain[Json], Json]] =
          NonEmptyChain
            .fromChain {
              path
                .segments
                .foldLeft[Chain[Json]](Chain.one(json))(_ <<: _)
            }
            .map { nel =>
              if (expectingArray) nel.asLeft
              else if (nel.tail.isEmpty) nel.head.asRight
              else Json.arr(nel.toNonEmptyVector.toVector: _*).asRight
            }

        // Special case: never accepts JSON that has nothing at the path
        if (withPath.when === WhenNever) pathResultsOpt.isEmpty
        else
          pathResultsOpt.exists {
            case Right(jsonResult) => jsonResult.satisfies(withPath.when)
            case Left(jsonResultNec) => jsonResultNec.exists(withPath.when.satisfiedBy(_))
          }
      }
  }

  implicit val eq: Eq[JsonTest] = Eq.instance { (a, b) =>
    a.pathOpt === b.pathOpt
  }
}

object JsonPredicate extends PredicateWrapper[Json, JsonTest] {
  private def noPath(p: Fixed[Json] |+| UsingEq[Json] |+| UsingOrder[Json]): Type =
    wrap(JsonTest(none[JsonPath], p).first[UsingCombinators[Base]])

  private def hasPath(path: JsonPath, p: Fixed[Json] |+| UsingEq[Json] |+| UsingOrder[Json]): Type =
    wrap(JsonTest(path.some, p).first[UsingCombinators[Base]])

  val always: Type = noPath(Fixed.Always[Json]().upcast.first[UsingEq[Json]].first[UsingOrder[Json]])

  def always(path: JsonPath): Type =
    hasPath(path, Fixed.Always[Json]().upcast.first[UsingEq[Json]].first[UsingOrder[Json]])

  val never: Type = noPath(Fixed.Never[Json]().upcast.first[UsingEq[Json]].first[UsingOrder[Json]])

  def never(path: JsonPath): Type =
    hasPath(path, Fixed.Never[Json]().upcast.first[UsingEq[Json]].first[UsingOrder[Json]])

  def is(sentinel: Json): Type = noPath(UsingEq.Is[Json](sentinel).upcast.second[Fixed[Json]].first[UsingOrder[Json]])

  def is(path: JsonPath, sentinel: Json): Type =
    hasPath(path, UsingEq.Is[Json](sentinel).upcast.second[Fixed[Json]].first[UsingOrder[Json]])

  def in(sentinels: List[Json]): Type =
    noPath(UsingEq.In[Json](sentinels).upcast.second[Fixed[Json]].first[UsingOrder[Json]])

  def in(path: JsonPath, sentinels: List[Json]): Type =
    hasPath(path, UsingEq.In[Json](sentinels).upcast.second[Fixed[Json]].first[UsingOrder[Json]])

  def lessThan(sentinel: Json): Type =
    noPath(UsingOrder.LessThan[Json](sentinel).upcast.second[Fixed[Json] |+| UsingEq[Json]])

  def lessThan(path: JsonPath, sentinel: Json): Type =
    hasPath(path, UsingOrder.LessThan[Json](sentinel).upcast.second[Fixed[Json] |+| UsingEq[Json]])

  def lessThanEq(sentinel: Json): Type =
    noPath(UsingOrder.LessThanEq[Json](sentinel).upcast.second[Fixed[Json] |+| UsingEq[Json]])

  def lessThanEq(path: JsonPath, sentinel: Json): Type =
    hasPath(path, UsingOrder.LessThanEq[Json](sentinel).upcast.second[Fixed[Json] |+| UsingEq[Json]])

  def greaterThan(sentinel: Json): Type =
    noPath(UsingOrder.GreaterThan[Json](sentinel).upcast.second[Fixed[Json] |+| UsingEq[Json]])

  def greaterThan(path: JsonPath, sentinel: Json): Type =
    hasPath(path, UsingOrder.GreaterThan[Json](sentinel).upcast.second[Fixed[Json] |+| UsingEq[Json]])

  def greaterThanEq(sentinel: Json): Type =
    noPath(UsingOrder.GreaterThanEq[Json](sentinel).upcast.second[Fixed[Json] |+| UsingEq[Json]])

  def greaterThanEq(path: JsonPath, sentinel: Json): Type =
    hasPath(path, UsingOrder.GreaterThanEq[Json](sentinel).upcast.second[Fixed[Json] |+| UsingEq[Json]])
}
