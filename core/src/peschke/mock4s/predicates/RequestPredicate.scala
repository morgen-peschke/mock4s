package peschke.mock4s.predicates

import cats.Eq
import cats.syntax.all._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}
import peschke.mock4s.algebras.PredicateChecker
import peschke.mock4s.algebras.PredicateChecker.syntax._
import peschke.mock4s.models.{ParsedRequest, |+|}
import peschke.mock4s.models.|+|.syntax._
import peschke.mock4s.predicates
import peschke.mock4s.predicates.RequestTest.{WhenBody, WhenHeaders, WhenRoute, WhenState}
import peschke.mock4s.utils.Circe._

sealed trait RequestTest {
  def upcast: RequestTest = this
}

object RequestTest {
  final case class WhenRoute(p: RoutePredicate.Type) extends RequestTest

  final case class WhenHeaders(px: List[HeaderPredicate.Type]) extends RequestTest

  final case class WhenBody(p: BodyPredicate.Type) extends RequestTest

  final case class WhenState(p: StatePredicate.Type) extends RequestTest

  implicit val decoder: Decoder[RequestTest] = anyOf[RequestTest](
    Decoder[RoutePredicate.Type].at("route").map(WhenRoute),
    Decoder[List[HeaderPredicate.Type]].at("headers").map(WhenHeaders),
    Decoder[BodyPredicate.Type].at("body").map(WhenBody),
    Decoder[StatePredicate.Type].at("state").map(WhenState)
  )

  implicit val encoder: Encoder[RequestTest] = Encoder.instance {
    case WhenRoute(p) => Json.obj("route" := p)
    case WhenHeaders(px) => Json.obj("headers" := px)
    case WhenBody(p) => Json.obj("body" := p)
    case WhenState(p) => Json.obj("state" := p)
  }

  implicit val eq: Eq[RequestTest] = Eq.instance {
    case (WhenRoute(a), WhenRoute(b)) => a === b
    case (WhenHeaders(a), WhenHeaders(b)) => a === b
    case (WhenBody(a), WhenBody(b)) => a === b
    case (WhenState(a), WhenState(b)) => a === b
    case _ => false
  }

  implicit val predicateChecker: PredicateChecker[ParsedRequest, RequestTest] =
    (predicate, in) => predicate match {
      case WhenRoute(p) => in.route.satisfies(p)
      case WhenHeaders(px) => px.forall(p => in.headers.exists(_.satisfies(p)))
      case WhenBody(p) => in.body.satisfies(p)
      case WhenState(p) => in.route.state.satisfies(p)
    }
}

object RequestPredicate extends PredicateWrapper[ParsedRequest, Fixed[ParsedRequest] |+| RequestTest] {
  val always: Type = wrap(Fixed.Always[ParsedRequest]().upcast.first[RequestTest].first[UsingCombinators[Base]])

  val never: Type = wrap(Fixed.Never[ParsedRequest]().upcast.first[RequestTest].first[UsingCombinators[Base]])

  def route(p: RoutePredicate.Type): Type =
    wrap(WhenRoute(p).upcast.second[Fixed[ParsedRequest]].first[UsingCombinators[Base]])

  def headers(px: List[HeaderPredicate.Type]): Type =
    wrap(WhenHeaders(px).upcast.second[Fixed[ParsedRequest]].first[UsingCombinators[Base]])

  def body(p: predicates.BodyPredicate.Type): Type =
    wrap(WhenBody(p).upcast.second[Fixed[ParsedRequest]].first[UsingCombinators[Base]])

  def state(p: StatePredicate.Type): Type =
    wrap(WhenState(p).upcast.second[Fixed[ParsedRequest]].first[UsingCombinators[Base]])
}
