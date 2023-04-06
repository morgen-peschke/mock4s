package peschke.mock4s.models

import cats.syntax.all._
import cats.{Eq, Show}
import io.circe.syntax._
import io.circe.{Decoder, DecodingFailure, Encoder}
import peschke.mock4s.utils.Circe._
import peschke.mock4s.utils.JsonPathParser

final case class JsonPath(segments: List[JsonPath.Segment], raw: String)
object JsonPath {
  sealed trait Segment
  object Segment {
    final case class DownField(name: String) extends Segment
    final case class AtIndex(value: Int) extends Segment
    case object DownArray extends Segment

    implicit val eq: Eq[Segment] = Eq.instance {
      case (DownField(a), DownField(b)) => a === b
      case (AtIndex(a), AtIndex(b)) => a === b
      case (DownArray, DownArray) => true
      case _ => false
    }
  }

  val Empty: JsonPath = JsonPath(Nil, "")

  implicit val show: Show[JsonPath] = Show.show(_.raw)

  implicit val eq: Eq[JsonPath] = Eq.instance { (a, b) =>
    a.raw === b.raw && a.segments === b.segments
  }

  implicit val decoder: Decoder[JsonPath] = accumulatingDecoder[JsonPath] { c =>
    c.asAcc[String].andThen {
      JsonPathParser.parse(_).leftMap(_.map(pe => DecodingFailure(pe.show, c.history)).toNonEmptyList)
    }
  }

  implicit val encoder: Encoder[JsonPath] = Encoder.instance(_.show.asJson)
}