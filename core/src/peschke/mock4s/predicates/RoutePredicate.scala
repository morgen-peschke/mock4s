package peschke.mock4s.predicates

import cats.Eq
import cats.syntax.all._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}
import peschke.mock4s.algebras.PredicateChecker
import peschke.mock4s.algebras.PredicateChecker.syntax._
import peschke.mock4s.models.ParsedRequest.Route
import peschke.mock4s.models.|+|
import peschke.mock4s.models.|+|.syntax._
import peschke.mock4s.predicates.RouteTests.{WhenMethod, WhenPath, WhenQuery, WhenState}
import peschke.mock4s.utils.Circe._

sealed trait RouteTests {
  def upcast: RouteTests = this
}

object RouteTests {
  final case class WhenMethod(predicate: MethodPredicate.Type) extends RouteTests

  final case class WhenPath(predicate: PathPredicate.Type) extends RouteTests

  final case class WhenQuery(predicate: QueryPredicate.Type) extends RouteTests

  final case class WhenState(predicate: StatePredicate.Type) extends RouteTests

  implicit val decoder: Decoder[RouteTests] = anyOf[RouteTests](
    Decoder[MethodPredicate.Type].at("method").map(WhenMethod),
    Decoder[PathPredicate.Type].at("path").map(WhenPath),
    Decoder[QueryPredicate.Type].at("query").map(WhenQuery),
    Decoder[StatePredicate.Type].at("state").map(WhenState)
  )

  implicit val encoder: Encoder[RouteTests] = Encoder.instance {
    case WhenMethod(predicate) => Json.obj("method" := predicate)
    case WhenPath(predicate) => Json.obj("path" := predicate)
    case WhenQuery(predicate) => Json.obj("query" := predicate)
    case WhenState(predicate) => Json.obj("state" := predicate)
  }

  implicit val eq: Eq[RouteTests] = Eq.instance {
    case (WhenMethod(a), WhenMethod(b)) => a === b
    case (WhenPath(a), WhenPath(b)) => a === b
    case (WhenQuery(a), WhenQuery(b)) => a === b
    case (WhenState(a), WhenState(b)) => a === b
    case _ => false
  }

  implicit val predicateChecker: PredicateChecker[Route, RouteTests] = (predicate, in) => predicate match {
    case WhenMethod(predicate) => in.method.satisfies(predicate)
    case WhenPath(predicate) => in.path.satisfies(predicate)
    case WhenQuery(predicate) => in.query.satisfies(predicate)
    case WhenState(predicate) => in.state.satisfies(predicate)
  }
}

object RoutePredicate       extends PredicateWrapper[Route, Fixed[Route] |+| UsingEq[Route] |+| RouteTests] {
  val always: Type =
    wrap(Fixed.Always[Route]().upcast.first[UsingEq[Route]].first[RouteTests].first[UsingCombinators[Base]])

  val never: Type =
    wrap(Fixed.Never[Route]().upcast.first[UsingEq[Route]].first[RouteTests].first[UsingCombinators[Base]])

  def is(sentinel: Route): Type =
    wrap(UsingEq.Is[Route](sentinel).upcast.second[Fixed[Route]].first[RouteTests].first[UsingCombinators[Base]])

  def in(sentinels: List[Route]): Type =
    wrap(UsingEq.In[Route](sentinels).upcast.second[Fixed[Route]].first[RouteTests].first[UsingCombinators[Base]])

  def method(p: MethodPredicate.Type): Type =
    wrap(WhenMethod(p).upcast.second[Fixed[Route] |+| UsingEq[Route]].first[UsingCombinators[Base]])

  def path(p: PathPredicate.Type): Type =
    wrap(WhenPath(p).upcast.second[Fixed[Route] |+| UsingEq[Route]].first[UsingCombinators[Base]])

  def query(p: QueryPredicate.Type): Type =
    wrap(WhenQuery(p).upcast.second[Fixed[Route] |+| UsingEq[Route]].first[UsingCombinators[Base]])

  def state(p: StatePredicate.Type): Type =
    wrap(WhenState(p).upcast.second[Fixed[Route] |+| UsingEq[Route]].first[UsingCombinators[Base]])
}
