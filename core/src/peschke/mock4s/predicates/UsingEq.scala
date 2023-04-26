package peschke.mock4s.predicates

import cats.Eq
import cats.syntax.all._
import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax._
import peschke.mock4s.algebras.PredicateChecker
import peschke.mock4s.utils.Circe._

sealed abstract class UsingEq[A] {
  def upcast: UsingEq[A] = this
}

object UsingEq {
  final case class Is[A](sentinel: A) extends UsingEq[A]

  object Is {
    implicit def decoder[A: Decoder]: Decoder[Is[A]] = Decoder[A].map(Is[A]).at("is")

    implicit def encoder[A: Encoder]: Encoder[Is[A]] =
      Encoder.instance(is => Json.obj("is" := is.sentinel))

    implicit def eq[A: Eq]: Eq[Is[A]] = Eq.by(_.sentinel)
  }

  final case class In[A](sentinels: List[A]) extends UsingEq[A]
  object In {
    implicit def decoder[A: Decoder]: Decoder[In[A]] = Decoder[List[A]].map(In[A]).at("in")

    implicit def encoder[A: Encoder]: Encoder[In[A]] =
      Encoder.instance(in => Json.obj("in" := in.sentinels))

    implicit def eq[A: Eq]: Eq[In[A]] = Eq.by(_.sentinels)
  }

  implicit def decoder[A: Decoder]: Decoder[UsingEq[A]] = anyOf[UsingEq[A]](
    Is.decoder[A].widen,
    In.decoder[A].widen
  )

  implicit def encoder[A: Encoder]: Encoder[UsingEq[A]] = Encoder.instance {
    case p @ Is(_) => p.asJson
    case p @ In(_) => p.asJson
  }

  implicit def eq[A: Eq]: Eq[UsingEq[A]] = Eq.instance {
    case (a: Is[A], b: Is[A]) => a === b
    case (a: In[A], b: In[A]) => a === b
    case _                    => false
  }

  implicit def checker[A: Eq]: PredicateChecker[A, UsingEq[A]] =
    (predicate, in) =>
      predicate match {
        case UsingEq.Is(sentinel)  => sentinel === in
        case UsingEq.In(sentinels) => sentinels.exists(_ === in)
      }
}
