package peschke.mock4s.predicates

import cats.Eq
import cats.syntax.all._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}
import peschke.mock4s.models.ParsedRequest
import peschke.mock4s.predicates
import peschke.mock4s.predicates.Predicate.{Fixed, UsingCombinators}
import peschke.mock4s.utils.Circe._

sealed trait RequestPredicate extends Predicate[ParsedRequest]
object RequestPredicate extends PredicateWrapper[ParsedRequest] {

  final case class WhenRoute(p: RoutePredicate.Type) extends RequestPredicate {
    override def test(a: ParsedRequest): Boolean = p.test(a.route)
  }

  final case class WhenHeaders(px: List[HeaderPredicate.Type]) extends RequestPredicate {
    override def test(a: ParsedRequest): Boolean = px.forall(p => a.headers.exists(p.test))
  }

  final case class WhenBody(p: BodyPredicate.Type) extends RequestPredicate {
    override def test(a: ParsedRequest): Boolean = p.test(a.body)
  }

  implicit val requestPredicateDecoder: Decoder[RequestPredicate] = anyOf[RequestPredicate](
    Decoder[RoutePredicate.Type].at("route").map(WhenRoute),
    Decoder[List[HeaderPredicate.Type]].at("headers").map(WhenHeaders),
    Decoder[BodyPredicate.Type].at("body").map(WhenBody)
  )

  implicit val requestPredicateEncoder: Encoder[RequestPredicate] = Encoder.instance {
    case WhenRoute(p) => Json.obj("route" := p)
    case WhenHeaders(px) => Json.obj("headers" := px)
    case WhenBody(p) => Json.obj("body" := p)
  }

  implicit val requestPredicateEq: Eq[RequestPredicate] = Eq.instance {
    case (WhenRoute(a), WhenRoute(b)) => a === b
    case (WhenHeaders(a), WhenHeaders(b)) => a === b
    case (WhenBody(a), WhenBody(b)) => a === b
    case _ => false
  }

  override type Base = Fixed[ParsedRequest] |+| RequestPredicate

  override implicit def baseDecoder: Decoder[Base] = GeneratedDecoder[Base].decoder
  override implicit def baseEncoder: Encoder[Base] = GeneratedEncoder[Base].encoder

  val always: Type = wrap(lhs[Base, UsingCombinators[ParsedRequest, Base]](
    lhs[Fixed[ParsedRequest], RequestPredicate](Fixed.Always[ParsedRequest]())
  ))

  val never: Type = wrap(lhs[Base, UsingCombinators[ParsedRequest, Base]](
    lhs[Fixed[ParsedRequest], RequestPredicate](Fixed.Never[ParsedRequest]())
  ))

  def route(p: RoutePredicate.Type): Type = wrap {
    lhs[Base, UsingCombinators[ParsedRequest, Base]](
      rhs[Fixed[ParsedRequest], RequestPredicate](WhenRoute(p))
    )
  }

  def headers(px: List[HeaderPredicate.Type]): Type = wrap {
    lhs[Base, UsingCombinators[ParsedRequest, Base]](
      rhs[Fixed[ParsedRequest], RequestPredicate](WhenHeaders(px))
    )
  }

  def body(p: predicates.BodyPredicate.Type): Type = wrap {
    lhs[Base, UsingCombinators[ParsedRequest, Base]](
      rhs[Fixed[ParsedRequest], RequestPredicate](WhenBody(p))
    )
  }
}
