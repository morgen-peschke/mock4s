package peschke.mock4s.predicates

import cats.Eq
import cats.syntax.all._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}
import org.http4s.{Method, Query}
import peschke.mock4s.models.ParsedRequest.Route
import peschke.mock4s.predicates.Predicate.{Fixed, UsingCombinators, UsingEq}
import peschke.mock4s.utils.Circe._

sealed trait RoutePredicate extends Predicate[Route] {
  def routePredicate: RoutePredicate = this
}
object RoutePredicate       extends PredicateWrapper[Route] {
  object MethodPredicate extends Predicate.SimpleEq[Method]
  object QueryPredicate  extends Predicate.SimpleEq[Query]

  final case class WhenMethod(predicate: MethodPredicate.Type) extends RoutePredicate {
    override def test(a: Route): Boolean = predicate.test(a.method)
  }

  final case class WhenPath(predicate: PathPredicate.Type) extends RoutePredicate {
    override def test(a: Route): Boolean = predicate.test(a.path)
  }

  final case class WhenQuery(predicate: QueryPredicate.Type) extends RoutePredicate {
    override def test(a: Route): Boolean = predicate.test(a.query)
  }

  final case class WhenState(p: StatePredicate.Type) extends RoutePredicate {
    override def test(a: Route): Boolean = p.test(a.state)
  }

  implicit val routePredicateDecoder: Decoder[RoutePredicate] = anyOf[RoutePredicate](
    Decoder[MethodPredicate.Type].at("method").map(WhenMethod),
    Decoder[PathPredicate.Type].at("path").map(WhenPath),
    Decoder[QueryPredicate.Type].at("query").map(WhenQuery),
    Decoder[StatePredicate.Type].at("state").map(WhenState)
  )

  implicit val routePredicateEncoder: Encoder[RoutePredicate] = Encoder.instance {
    case WhenMethod(predicate) => Json.obj("method" := predicate)
    case WhenPath(predicate)   => Json.obj("path" := predicate)
    case WhenQuery(predicate)  => Json.obj("query" := predicate)
    case WhenState(predicate)  => Json.obj("state" := predicate)
  }

  implicit val routePredicateEq: Eq[RoutePredicate] = Eq.instance {
    case (WhenMethod(a), WhenMethod(b)) => a === b
    case (WhenPath(a), WhenPath(b))     => a === b
    case (WhenQuery(a), WhenQuery(b))   => a === b
    case (WhenState(a), WhenState(b))   => a === b
    case _                              => false
  }

  override type Base = Fixed[Route] |+| UsingEq[Route] |+| RoutePredicate

  override implicit val baseDecoder: Decoder[Base] = GeneratedDecoder[Base].decoder
  override implicit val baseEncoder: Encoder[Base] = GeneratedEncoder[Base].encoder

  val always: Type = wrap {
    lhs[Base, UsingCombinators[Route, Base]](
      lhs[Fixed[Route] |+| UsingEq[Route], RoutePredicate](
        lhs[Fixed[Route], UsingEq[Route]](Fixed.Always[Route]())
      )
    )
  }

  val never: Type = wrap {
    lhs[Base, UsingCombinators[Route, Base]](
      lhs[Fixed[Route] |+| UsingEq[Route], RoutePredicate](
        lhs[Fixed[Route], UsingEq[Route]](Fixed.Never[Route]())
      )
    )
  }

  def is(sentinel: Route): Type = wrap {
    lhs[Base, UsingCombinators[Route, Base]](
      lhs[Fixed[Route] |+| UsingEq[Route], RoutePredicate](
        rhs[Fixed[Route], UsingEq[Route]](UsingEq.Is[Route](sentinel))
      )
    )
  }

  def in(sentinels: List[Route]): Type = wrap {
    lhs[Base, UsingCombinators[Route, Base]](
      lhs[Fixed[Route] |+| UsingEq[Route], RoutePredicate](
        rhs[Fixed[Route], UsingEq[Route]](UsingEq.In[Route](sentinels))
      )
    )
  }

  def method(p: MethodPredicate.Type): Type = wrap {
    lhs[Base, UsingCombinators[Route, Base]](
      rhs[Fixed[Route] |+| UsingEq[Route], RoutePredicate](WhenMethod(p))
    )
  }

  def path(p: PathPredicate.Type): Type = wrap {
    lhs[Base, UsingCombinators[Route, Base]](
      rhs[Fixed[Route] |+| UsingEq[Route], RoutePredicate](WhenPath(p))
    )
  }

  def query(p: QueryPredicate.Type): Type = wrap {
    lhs[Base, UsingCombinators[Route, Base]](
      rhs[Fixed[Route] |+| UsingEq[Route], RoutePredicate](WhenQuery(p))
    )
  }

  def state(p: StatePredicate.Type): Type =
    WhenState(p)
      .routePredicate
      .rhs[Fixed[Route] |+| UsingEq[Route]]
      .lhs[UsingCombinators[Route, Base]]
      .wrapped
}
