package peschke.mock4s.predicates

import cats.syntax.all._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}
import peschke.mock4s.models.Body.HexString
import peschke.mock4s.models.ParsedBody
import peschke.mock4s.utils.Circe._

sealed abstract class BodyPredicate extends Predicate[ParsedBody] {
  type PredStr <: Predicate[String]
  type PredJs <: Predicate[Json]
  type PredByte <: Predicate[HexString]
}
object BodyPredicate {
  type Aux[
    PS <: Predicate[String],
    PJ <: Predicate[Json],
    PB <: Predicate[HexString]
  ] = BodyPredicate {
    type PredStr = PS
    type PredJs = PJ
    type PredByte = PB
  }

  object NoSpecialJsonPredicate extends Predicate.Simple[Json]
  object NoSpecialBytePredicate extends Predicate.Simple[HexString]

  type Default = BodyPredicate {
    type PredStr = StandardStringPredicate
    type PredJs = NoSpecialJsonPredicate.type
    type PredByte = NoSpecialBytePredicate.type
  }

  case class CannotBeDecoded[
    PS <: Predicate[String],
    PJ <: Predicate[Json],
    PB <: Predicate[HexString]
  ]() extends BodyPredicate {
    override type PredStr = PS
    override type PredJs = PJ
    override type PredByte = PB

    override def test(a: ParsedBody): Boolean = false
  }

  object CannotBeDecoded {
    implicit def encoder[
      PS <: Predicate[String],
      PJ <: Predicate[Json],
      PB <: Predicate[HexString]
    ]: Encoder[CannotBeDecoded[PS, PJ, PB]] = Encoder.instance(_ => "cannot-be-decoded".asJson)
  }

  case class IsEmpty[
    PS <: Predicate[String],
    PJ <: Predicate[Json],
    PB <: Predicate[HexString]
  ]() extends BodyPredicate {
    override type PredStr = PS
    override type PredJs = PJ
    override type PredByte = PB

    override def test(a: ParsedBody): Boolean = a match {
      case ParsedBody.EmptyBody => true
    }
  }
  object IsEmpty {
    implicit def decoder[
      PS <: Predicate[String],
      PJ <: Predicate[Json],
      PB <: Predicate[HexString]
    ]: Decoder[IsEmpty[PS, PJ, PB]] = fixed("empty").as(IsEmpty[PS, PJ, PB]())

    implicit def encoder[
      PS <: Predicate[String],
      PJ <: Predicate[Json],
      PB <: Predicate[HexString]
    ]: Encoder[IsEmpty[PS, PJ, PB]] = Encoder.instance(_ => "empty".asJson)
  }

  final case class TextPredicate[
    PS <: Predicate[String],
    PJ <: Predicate[Json],
    PB <: Predicate[HexString]
  ](pred: PredicateADT.Aux[String, PS]) extends BodyPredicate {
    override type PredStr = PS
    override type PredJs = PJ
    override type PredByte = PB

    override def test(a: ParsedBody): Boolean = a match {
      case ParsedBody.TextBody(text, _, _) => pred.test(text)
      case ParsedBody.JsonBody(_, text, _) => pred.test(text)
      case _ => false
    }
  }
  object TextPredicate {
    implicit def decoder[
      PS <: Predicate[String],
      PJ <: Predicate[Json],
      PB <: Predicate[HexString]
    ](implicit predDecoder: Decoder[PredicateADT.Aux[String, PS]]): Decoder[TextPredicate[PS, PJ, PB]] =
      predDecoder.map(TextPredicate[PS, PJ, PB]).at("text")

    implicit def encoder[
      PS <: Predicate[String],
      PJ <: Predicate[Json],
      PB <: Predicate[HexString]
    ](implicit predEncoder: Encoder[PredicateADT.Aux[String, PS]]): Encoder[TextPredicate[PS, PJ, PB]] =
      Encoder.instance(tp => Json.obj("text" := tp.pred))
  }

  final case class JsonPredicate[
    PS <: Predicate[String],
    PJ <: Predicate[Json],
    PB <: Predicate[HexString]
  ](pred: PredicateADT.Aux[Json, PJ]) extends BodyPredicate {
    override type PredStr = PS
    override type PredJs = PJ
    override type PredByte = PB

    override def test(a: ParsedBody): Boolean = a match {
      case ParsedBody.JsonBody(json, _, _) => pred.test(json)
      case _ => false
    }
  }
  object JsonPredicate {
    implicit def decoder[
      PS <: Predicate[String],
      PJ <: Predicate[Json],
      PB <: Predicate[HexString]
    ](implicit predDecoder: Decoder[PredicateADT.Aux[Json, PJ]]): Decoder[JsonPredicate[PS, PJ, PB]] =
      predDecoder.map(JsonPredicate[PS, PJ, PB]).at("json")

    implicit def encoder[
      PS <: Predicate[String],
      PJ <: Predicate[Json],
      PB <: Predicate[HexString]
    ](implicit predEncoder: Encoder[PredicateADT.Aux[Json, PJ]]): Encoder[JsonPredicate[PS, PJ, PB]] =
      Encoder.instance(tp => Json.obj("json" := tp.pred))
  }

  final case class RawPredicate[
    PS <: Predicate[String],
    PJ <: Predicate[Json],
    PB <: Predicate[HexString]
  ](pred: PredicateADT.Aux[HexString, PB]) extends BodyPredicate {
    override type PredStr = PS
    override type PredJs = PJ
    override type PredByte = PB

    override def test(a: ParsedBody): Boolean = a match {
      case ParsedBody.JsonBody(_, _, bytes) => pred.test(bytes)
      case ParsedBody.TextBody(_, bytes, _) => pred.test(bytes)
      case ParsedBody.RawBody(bytes, _) => pred.test(bytes)
      case _ => false
    }
  }
  object RawPredicate {
    implicit def decoder[
      PS <: Predicate[String],
      PJ <: Predicate[Json],
      PB <: Predicate[HexString]
    ](implicit predDecoder: Decoder[PredicateADT.Aux[HexString, PB]]): Decoder[RawPredicate[PS, PJ, PB]] =
      predDecoder.map(RawPredicate[PS, PJ, PB]).at("raw")

    implicit def encoder[
      PS <: Predicate[String],
      PJ <: Predicate[Json],
      PB <: Predicate[HexString]
    ](implicit predEncoder: Encoder[PredicateADT.Aux[HexString, PB]]): Encoder[RawPredicate[PS, PJ, PB]] =
      Encoder.instance(tp => Json.obj("raw" := tp.pred))
  }

  implicit def decoder[
    PS <: Predicate[String],
    PJ <: Predicate[Json],
    PB <: Predicate[HexString]
  ](implicit
                       strPred: Decoder[PredicateADT.Aux[String, PS]],
                       jsonPred: Decoder[PredicateADT.Aux[Json, PJ]],
                       bytePred: Decoder[PredicateADT.Aux[HexString, PB]]
                      ): Decoder[BodyPredicate.Aux[PS, PJ, PB]] = anyOf[BodyPredicate.Aux[PS,PJ,PB]](
    IsEmpty.decoder[PS, PJ, PB].widen,
    TextPredicate.decoder[PS, PJ, PB].widen,
    JsonPredicate.decoder[PS, PJ, PB].widen,
    RawPredicate.decoder[PS, PJ, PB].widen,
  )

  implicit def encoder[
    PS <: Predicate[String],
    PJ <: Predicate[Json],
    PB <: Predicate[HexString]
  ](implicit
    strPred: Encoder[PredicateADT.Aux[String, PS]],
    jsonPred: Encoder[PredicateADT.Aux[Json, PJ]],
    bytePred: Encoder[PredicateADT.Aux[HexString, PB]]
   ): Encoder[BodyPredicate.Aux[PS, PJ, PB]] = Encoder.instance {
    case pb @ IsEmpty() => pb.asJson
    case pb @ TextPredicate(_) => pb.asJson
    case pb @ JsonPredicate(_) => pb.asJson
    case pb @ RawPredicate(_) => pb.asJson
    case pb @ CannotBeDecoded() => pb.asJson
  }
}
