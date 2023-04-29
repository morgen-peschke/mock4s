package peschke.mock4s.models

import cats.syntax.all._
import io.circe._
import io.circe.syntax._
import peschke.mock4s.utils.Circe._

final case class JsonObjectTuple[A, B](key: A, value: B) {
  def mapN[C](f: (A, B) => C): C = f(key, value)

  def json(implicit KE: KeyEncoder[A], VE: Encoder[B]): Json = JsonObjectTuple.json(key, value)
}

object JsonObjectTuple {
  def json[A: KeyEncoder, B: Encoder](key: A, value: B): Json = Json.obj(key := value)

  implicit def decoder[A: KeyDecoder, B: Decoder]: Decoder[JsonObjectTuple[A, B]] = accumulatingDecoder { c =>
    c.asAcc[Map[A, B]].andThen { map =>
      map.toList match {
        case (key, value) :: Nil => JsonObjectTuple(key, value).valid
        case _ => DecodingFailure("Expected a map with a single key/value pair", c.history).invalidNel
      }
    }
  }

  implicit def encoder[A: KeyEncoder, B: Encoder]: Encoder[JsonObjectTuple[A, B]] =
    Encoder.instance[JsonObjectTuple[A, B]](_.json)
}