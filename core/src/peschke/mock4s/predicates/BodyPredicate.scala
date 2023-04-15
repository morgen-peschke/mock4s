package peschke.mock4s.predicates

import cats.Eq
import cats.syntax.all._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}
import peschke.mock4s.models.Body.Base64String
import peschke.mock4s.models.ParsedBody
import peschke.mock4s.predicates.Predicate.{Fixed, UsingCombinators}
import peschke.mock4s.utils.Circe._

sealed trait BodyPredicate extends Predicate[ParsedBody]
object BodyPredicate       extends PredicateWrapper[ParsedBody] {
  case object IsEmpty extends BodyPredicate {
    override def test(a: ParsedBody): Boolean = a match {
      case ParsedBody.EmptyBody => true
      case _                    => false
    }
  }

  final case class TextBodyPredicate(p: StringPredicate.Type) extends BodyPredicate {
    override def test(a: ParsedBody): Boolean = a match {
      case ParsedBody.TextBody(text, _, _) => p.test(text)
      case ParsedBody.JsonBody(_, text, _) => p.test(text)
      case _                               => false
    }
  }

  final case class JsonBodyPredicate(p: JsonPredicate.Type) extends BodyPredicate {
    override def test(a: ParsedBody): Boolean = a match {
      case ParsedBody.JsonBody(json, _, _) => p.test(json)
      case _                               => false
    }
  }

  final case class RawBodyPredicate(p: StringPredicate.Type) extends BodyPredicate {
    override def test(a: ParsedBody): Boolean = a match {
      case ParsedBody.JsonBody(_, _, bytes) => p.test(Base64String.raw(bytes))
      case ParsedBody.TextBody(_, bytes, _) => p.test(Base64String.raw(bytes))
      case ParsedBody.RawBody(bytes, _)     => p.test(Base64String.raw(bytes))
      case _                                => false
    }
  }

  implicit val bodyPredicateDecoder: Decoder[BodyPredicate] = anyOf[BodyPredicate](
    fixed("empty").as(IsEmpty),
    Decoder[StringPredicate.Type].at("text").map(TextBodyPredicate),
    Decoder[JsonPredicate.Type].at("json").map(JsonBodyPredicate),
    Decoder[StringPredicate.Type].at("raw").map(RawBodyPredicate)
  )

  implicit val bodyPredicateEncoder: Encoder[BodyPredicate] = Encoder.instance {
    case IsEmpty              => "empty".asJson
    case TextBodyPredicate(p) => Json.obj("text" := p)
    case JsonBodyPredicate(p) => Json.obj("json" := p)
    case RawBodyPredicate(p)  => Json.obj("raw" := p)
  }

  implicit val bodyPredicateEq: Eq[BodyPredicate] = Eq.instance {
    case (IsEmpty, IsEmpty)                           => true
    case (TextBodyPredicate(a), TextBodyPredicate(b)) => a === b
    case (JsonBodyPredicate(a), JsonBodyPredicate(b)) => a === b
    case (RawBodyPredicate(a), RawBodyPredicate(b))   => a === b
    case _                                            => false
  }

  override type Base = Fixed[ParsedBody] |+| BodyPredicate

  override implicit def baseDecoder: Decoder[Base] = GeneratedDecoder[Base].decoder

  override implicit def baseEncoder: Encoder[Base] = GeneratedEncoder[Base].encoder

  val always: Type = wrap {
    lhs[Base, UsingCombinators[ParsedBody, Base]](
      lhs[Fixed[ParsedBody], BodyPredicate](Fixed.Always[ParsedBody]())
    )
  }

  val never: Type = wrap {
    lhs[Base, UsingCombinators[ParsedBody, Base]](
      lhs[Fixed[ParsedBody], BodyPredicate](Fixed.Never[ParsedBody]())
    )
  }

  val isEmpty: Type = wrap {
    lhs[Base, UsingCombinators[ParsedBody, Base]](
      rhs[Fixed[ParsedBody], BodyPredicate](IsEmpty)
    )
  }

  def text(p: StringPredicate.Type): Type = wrap {
    lhs[Base, UsingCombinators[ParsedBody, Base]](
      rhs[Fixed[ParsedBody], BodyPredicate](TextBodyPredicate(p))
    )
  }

  def json(p: JsonPredicate.Type): Type = wrap {
    lhs[Base, UsingCombinators[ParsedBody, Base]](
      rhs[Fixed[ParsedBody], BodyPredicate](JsonBodyPredicate(p))
    )
  }

  def raw(p: StringPredicate.Type): Type = wrap {
    lhs[Base, UsingCombinators[ParsedBody, Base]](
      rhs[Fixed[ParsedBody], BodyPredicate](RawBodyPredicate(p))
    )
  }
}
