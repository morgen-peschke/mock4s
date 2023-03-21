package peschke.mock4s.predicates

import cats.syntax.all._
import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax._
import org.http4s.Header
import org.typelevel.ci.CIString
import peschke.mock4s.predicates.HeaderPredicate.ValuePredicate
import peschke.mock4s.utils.Circe._

case class HeaderPredicate[PS <: Predicate[String]](name: CIString, value: ValuePredicate[PS]) extends Predicate[Header.Raw] {
  override def test(a: Header.Raw): Boolean = a.name === name && value.test(a)
}
object HeaderPredicate {
  type Default = HeaderPredicate[StandardStringPredicate]

  final case class ValuePredicate[PS <: Predicate[String]](sp: PredicateADT.Aux[String, PS]) extends Predicate[Header.Raw] {
    override def test(a: Header.Raw): Boolean = sp.test(a.value)
  }
  object ValuePredicate {
    implicit def decoder[PS <: Predicate[String]](implicit predDecoder: Decoder[PredicateADT.Aux[String, PS]]): Decoder[ValuePredicate[PS]] =
      predDecoder.map(ValuePredicate[PS]).at("value")

    implicit def encoder[PS <: Predicate[String]](implicit predEncoder: Encoder[PredicateADT.Aux[String, PS]]): Encoder[ValuePredicate[PS]] =
      Encoder.instance(vp => Json.obj("value" := vp.sp))
  }

  implicit def decoder[PS <: Predicate[String]](implicit predDecoder: Decoder[PredicateADT.Aux[String, PS]]): Decoder[HeaderPredicate[PS]] =
    accumulatingDecoder { c =>
      (
        c.downField("name").asAcc[CIString],
        c.downField("value").asAcc[ValuePredicate[PS]]
      ).mapN(HeaderPredicate.apply)
    }

  implicit def encoder[PS <: Predicate[String]](implicit predEncoder: Encoder[PredicateADT.Aux[String, PS]]): Encoder[HeaderPredicate[PS]] =
    Encoder.instance { hp =>
      Json.obj("name" := hp.name, "value" := ValuePredicate.encoder[PS].apply(hp.value))
    }
}