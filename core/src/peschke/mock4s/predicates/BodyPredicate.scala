package peschke.mock4s.predicates

import cats.Eq
import cats.syntax.all._
import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax._
import peschke.mock4s.algebras.PredicateChecker
import peschke.mock4s.algebras.PredicateChecker.syntax._
import peschke.mock4s.models.Body.Base64String
import peschke.mock4s.models.ParsedBody
import peschke.mock4s.models.|+|
import peschke.mock4s.models.|+|.syntax._
import peschke.mock4s.predicates.BodyTest.IsEmpty
import peschke.mock4s.predicates.BodyTest.JsonBodyPredicate
import peschke.mock4s.predicates.BodyTest.RawBodyPredicate
import peschke.mock4s.predicates.BodyTest.TextBodyPredicate
import peschke.mock4s.utils.Circe._

sealed trait BodyTest {
  def upcast: BodyTest = this
}
object BodyTest       {
  case object IsEmpty extends BodyTest

  final case class TextBodyPredicate(p: StringPredicate.Type) extends BodyTest

  final case class JsonBodyPredicate(p: JsonPredicate.Type) extends BodyTest

  final case class RawBodyPredicate(p: StringPredicate.Type) extends BodyTest

  implicit val decoder: Decoder[BodyTest] = anyOf[BodyTest](
    fixed("empty").as(IsEmpty),
    Decoder[StringPredicate.Type].at("text").map(TextBodyPredicate),
    Decoder[JsonPredicate.Type].at("json").map(JsonBodyPredicate),
    Decoder[StringPredicate.Type].at("raw").map(RawBodyPredicate)
  )

  implicit val encoder: Encoder[BodyTest] = Encoder.instance {
    case IsEmpty              => Json.fromString("empty")
    case TextBodyPredicate(p) => Json.obj("text" := p)
    case JsonBodyPredicate(p) => Json.obj("json" := p)
    case RawBodyPredicate(p)  => Json.obj("raw" := p)
  }

  implicit val eq: Eq[BodyTest] = Eq.instance {
    case (IsEmpty, IsEmpty)                           => true
    case (TextBodyPredicate(a), TextBodyPredicate(b)) => a === b
    case (JsonBodyPredicate(a), JsonBodyPredicate(b)) => a === b
    case (RawBodyPredicate(a), RawBodyPredicate(b))   => a === b
    case _                                            => false
  }

  implicit val checker: PredicateChecker[ParsedBody, BodyTest] = (predicate, in) =>
    predicate match {
      case IsEmpty              =>
        in match {
          case ParsedBody.EmptyBody => true
          case _                    => false
        }
      case TextBodyPredicate(p) =>
        in match {
          case ParsedBody.TextBody(text, _, _) => text.satisfies(p)
          case ParsedBody.JsonBody(_, text, _) => text.satisfies(p)
          case _                               => false
        }
      case JsonBodyPredicate(p) =>
        in match {
          case ParsedBody.JsonBody(json, _, _) => json.satisfies(p)
          case _                               => false
        }
      case RawBodyPredicate(p)  =>
        in match {
          case ParsedBody.JsonBody(_, _, bytes) => Base64String.raw(bytes).satisfies(p)
          case ParsedBody.TextBody(_, bytes, _) => Base64String.raw(bytes).satisfies(p)
          case ParsedBody.RawBody(bytes, _)     => Base64String.raw(bytes).satisfies(p)
          case _                                => false
        }
    }
}

object BodyPredicate extends PredicateWrapper[ParsedBody, Fixed[ParsedBody] |+| BodyTest] {
  val always: Type = wrap {
    Fixed.Always[ParsedBody]().upcast.first[BodyTest].first[UsingCombinators[Base]]
  }

  val never: Type = wrap {
    Fixed.Never[ParsedBody]().upcast.first[BodyTest].first[UsingCombinators[Base]]
  }

  val isEmpty: Type = wrap {
    IsEmpty.upcast.second[Fixed[ParsedBody]].first[UsingCombinators[Base]]
  }

  def text(p: StringPredicate.Type): Type = wrap {
    TextBodyPredicate(p).upcast.second[Fixed[ParsedBody]].first[UsingCombinators[Base]]
  }

  def json(p: JsonPredicate.Type): Type = wrap {
    JsonBodyPredicate(p).upcast.second[Fixed[ParsedBody]].first[UsingCombinators[Base]]
  }

  def bytes(p: StringPredicate.Type): Type = wrap {
    RawBodyPredicate(p).upcast.second[Fixed[ParsedBody]].first[UsingCombinators[Base]]
  }
}
