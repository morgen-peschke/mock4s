package peschke.mock4s.predicates

import cats.Eq
import cats.syntax.all._
import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax._
import org.http4s.Query
import peschke.mock4s.algebras.PredicateChecker
import peschke.mock4s.algebras.PredicateChecker.syntax._
import peschke.mock4s.models.|+|
import peschke.mock4s.models.|+|.syntax.LiftOps
import peschke.mock4s.utils.Circe._

sealed trait QueryTest {
  def upcast: QueryTest = this
}
object QueryTest       {
  case object Empty                                                extends QueryTest
  final case class OmitsParam(name: String)                        extends QueryTest
  final case class EmptyParam(name: String)                        extends QueryTest
  final case class ForParam(name: String, p: StringPredicate.Type) extends QueryTest

  implicit val decoder: Decoder[QueryTest] = anyOf[QueryTest](
    fixed("empty").as(Empty),
    Decoder[String].at("omits").map(OmitsParam),
    Decoder[String].at("empty-param").map(EmptyParam),
    Decoder[JsonObjectTuple[String, StringPredicate.Type]].at("param").map(_.mapN(ForParam))
  )

  implicit val encoder: Encoder[QueryTest] = Encoder.instance {
    case Empty            => Json.fromString("empty")
    case OmitsParam(key)  => Json.obj("omits" := key)
    case EmptyParam(key)  => Json.obj("empty-param" := key)
    case ForParam(key, p) => Json.obj("param" := JsonObjectTuple.json(key, p))
  }

  implicit val eq: Eq[QueryTest] = Eq.instance {
    case (Empty, Empty)                       => true
    case (OmitsParam(a), OmitsParam(b))       => a === b
    case (EmptyParam(a), EmptyParam(b))       => a === b
    case (ForParam(ak, ap), ForParam(bk, bp)) => ak === bk && ap === bp
    case _                                    => false
  }

  private def queryIsPresent(query: Query): Boolean = query match {
    case Query.Empty => false
    case _           => true
  }

  implicit val checker: PredicateChecker[Query, QueryTest] = (predicate, in) =>
    predicate match {
      case Empty            => !queryIsPresent(in)
      case OmitsParam(key)  => queryIsPresent(in) && !in.pairs.exists(key === _._1)
      case EmptyParam(key)  =>
        in.exists {
          case (k, None) => k === key
          case _         => false
        }
      case ForParam(key, p) =>
        in.exists {
          case (k, Some(v)) => k === key && v.satisfies(p)
          case _            => false
        }
    }
}

object QueryPredicate extends PredicateWrapper[Query, Fixed[Query] |+| UsingEq[Query] |+| QueryTest] {
  val always: Type =
    wrap(Fixed.Always[Query]().upcast.first[UsingEq[Query]].first[QueryTest].first[UsingCombinators[Base]])

  val never: Type =
    wrap(Fixed.Never[Query]().upcast.first[UsingEq[Query]].first[QueryTest].first[UsingCombinators[Base]])

  def is(sentinel: Query): Type = wrap {
    UsingEq
      .Is[Query](sentinel)
      .upcast
      .second[Fixed[Query]]
      .first[QueryTest]
      .first[UsingCombinators[Base]]
  }

  def in(sentinels: List[Query]): Type = wrap {
    UsingEq
      .In[Query](sentinels)
      .upcast
      .second[Fixed[Query]]
      .first[QueryTest]
      .first[UsingCombinators[Base]]
  }

  val empty: Type =
    wrap(QueryTest.Empty.upcast.second[Fixed[Query] |+| UsingEq[Query]].first[UsingCombinators[Base]])

  def omitsParam(name: String): Type =
    wrap(QueryTest.OmitsParam(name).upcast.second[Fixed[Query] |+| UsingEq[Query]].first[UsingCombinators[Base]])

  def emptyParam(name: String): Type =
    wrap(QueryTest.EmptyParam(name).upcast.second[Fixed[Query] |+| UsingEq[Query]].first[UsingCombinators[Base]])

  def forParam(name: String, p: StringPredicate.Type): Type =
    wrap(QueryTest.ForParam(name, p).upcast.second[Fixed[Query] |+| UsingEq[Query]].first[UsingCombinators[Base]])
}
