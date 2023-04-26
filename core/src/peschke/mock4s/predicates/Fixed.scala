package peschke.mock4s.predicates

import cats.Eq
import cats.syntax.all._
import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax._
import peschke.mock4s.algebras.PredicateChecker
import peschke.mock4s.utils.Circe._

sealed trait Fixed[A] {
  def upcast: Fixed[A] = this
}

object Fixed {
  final case class Always[A]() extends Fixed[A]
  object Always {
    implicit def decoder[A]: Decoder[Always[A]] =
      fixed("any").combine(fixed("always")).as(Always[A]())

    implicit def encoder[A]: Encoder[Always[A]] =
      Encoder.instance(_ => Json.fromString("any"))

    implicit def eq[A]: Eq[Always[A]] = Eq.instance((_, _) => true)
  }

  final case class Never[A]() extends Fixed[A]
  object Never {
    implicit def decoder[A]: Decoder[Never[A]] =
      fixed("fail").combine(fixed("never")).as(Never[A]())

    implicit def encoder[A]: Encoder[Never[A]] =
      Encoder.instance(_ => Json.fromString("fail"))

    implicit def eq[A]: Eq[Never[A]] = Eq.instance((_, _) => true)
  }

  implicit def decoder[A]: Decoder[Fixed[A]] = anyOf[Fixed[A]](
    Always.decoder[A].widen,
    Never.decoder[A].widen
  )

  implicit def encoder[A]: Encoder[Fixed[A]] = Encoder.instance {
    case p @ Always() => p.asJson
    case p @ Never()  => p.asJson
  }

  implicit def eq[A]: Eq[Fixed[A]] = Eq.instance {
    case (Always(), Always()) => true
    case (Never(), Never())   => true
    case _                    => false
  }

  implicit def predicateChecker[In]: PredicateChecker[In, Fixed[In]] = (predicate, _) =>
    predicate match {
      case Fixed.Always() => true
      case Fixed.Never()  => false
    }
}
