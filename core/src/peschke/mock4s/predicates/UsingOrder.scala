package peschke.mock4s.predicates

import cats.syntax.all._
import cats.{Eq, PartialOrder}
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}
import peschke.mock4s.algebras.PredicateChecker
import peschke.mock4s.utils.Circe._

sealed abstract class UsingOrder[A] {
  def upcast: UsingOrder[A] = this
}

object UsingOrder {
  final case class LessThan[A](sentinel: A) extends UsingOrder[A]

  object LessThan {
    implicit def decoder[A: Decoder ]: Decoder[LessThan[A]] = Decoder[A].map(LessThan[A]).at("<")

    implicit def encoder[A: Encoder]: Encoder[LessThan[A]] =
      Encoder.instance(in => Json.obj("<" := in.sentinel))

    implicit def eq[A: Eq]: Eq[LessThan[A]] = Eq.by(_.sentinel)
  }

  final case class LessThanEq[A](sentinel: A) extends UsingOrder[A]

  object LessThanEq {
    implicit def decoder[A: Decoder]: Decoder[LessThanEq[A]] = Decoder[A].map(LessThanEq[A]).at("<=")

    implicit def encoder[A: Encoder]: Encoder[LessThanEq[A]] =
      Encoder.instance(in => Json.obj("<=" := in.sentinel))

    implicit def eq[A: Eq]: Eq[LessThanEq[A]] = Eq.by(_.sentinel)
  }

  final case class GreaterThan[A](sentinel: A) extends UsingOrder[A]

  object GreaterThan {
    implicit def decoder[A: Decoder]: Decoder[GreaterThan[A]] = Decoder[A].map(GreaterThan[A]).at(">")

    implicit def encoder[A: Encoder]: Encoder[GreaterThan[A]] =
      Encoder.instance(in => Json.obj(">" := in.sentinel))

    implicit def eq[A: Eq]: Eq[GreaterThan[A]] = Eq.by(_.sentinel)
  }

  final case class GreaterThanEq[A](sentinel: A) extends UsingOrder[A]

  object GreaterThanEq {
    implicit def decoder[A: Decoder]: Decoder[GreaterThanEq[A]] =
      Decoder[A].map(GreaterThanEq[A]).at(">=")

    implicit def encoder[A: Encoder]: Encoder[GreaterThanEq[A]] =
      Encoder.instance(in => Json.obj(">=" := in.sentinel))

    implicit def eq[A: Eq]: Eq[GreaterThanEq[A]] = Eq.by(_.sentinel)
  }

  implicit def decoder[A: Decoder]: Decoder[UsingOrder[A]] = anyOf[UsingOrder[A]](
    LessThan.decoder[A].widen,
    LessThanEq.decoder[A].widen,
    GreaterThan.decoder[A].widen,
    GreaterThanEq.decoder[A].widen
  )

  implicit def encoder[A: Encoder]: Encoder[UsingOrder[A]] = Encoder.instance {
    case p@LessThan(_) => p.asJson
    case p@LessThanEq(_) => p.asJson
    case p@GreaterThan(_) => p.asJson
    case p@GreaterThanEq(_) => p.asJson
  }

  implicit def eq[A: Eq]: Eq[UsingOrder[A]] = Eq.instance {
    case (a: LessThan[A], b: LessThan[A]) => a === b
    case (a: LessThanEq[A], b: LessThanEq[A]) => a === b
    case (a: GreaterThan[A], b: GreaterThan[A]) => a === b
    case (a: GreaterThanEq[A], b: GreaterThanEq[A]) => a === b
    case _ => false
  }

  implicit def checker[A: PartialOrder]: PredicateChecker[A, UsingOrder[A]] =
    (predicate, in) => predicate match {
      case LessThan(sentinel) => in < sentinel
      case LessThanEq(sentinel) => in <= sentinel
      case GreaterThan(sentinel) => in > sentinel
      case GreaterThanEq(sentinel) => in >= sentinel
    }
}

