package peschke.mock4s.models

import cats.syntax.all._
import cats.{Eq, Show}
import io.circe.syntax._
import io.circe.{Decoder, Encoder}
import peschke.mock4s.algebras.PredicateChecker
import peschke.mock4s.utils.Circe._

sealed trait |+| [+First, +Second] {
    def fold[A](ff: First => A, sf: Second => A): A
}

object |+| {
  final case class First[F, S](first: F) extends (F |+| S) {
    override def fold[A](ff: F => A, sf: S => A): A = ff(first)
  }
  final case class Second[F, S](second: S) extends (F |+| S) {
    override def fold[A](ff: F => A, sf: S => A): A = sf(second)
  }

  implicit def eq[F: Eq, S: Eq]: Eq[F |+| S] = Eq.instance {
    case (First(a), First(b)) => a === b
    case (Second(a), Second(b)) => a === b
    case _ => false
  }

  implicit def show[F: Show, S: Show]: Show[F |+| S] = Show.show {
    _.fold(l => show"First($l)", r => show"Second($r)")
  }

  implicit def decoder[F: Decoder, S: Decoder]: Decoder[F |+| S] =
    anyOf[F |+| S](
      Decoder[F].map(First[F, S]).widen,
      Decoder[S].map(Second[F, S]).widen
    )

  implicit def encoder[F: Encoder, S: Encoder]: Encoder[F |+| S] =
    Encoder.instance[F |+| S](_.fold(_.asJson, _.asJson))

  implicit def predicateChecker[In, F, S](implicit FPC: PredicateChecker[In, F], SPC: PredicateChecker[In, S])
  : PredicateChecker[In, F |+| S] = (predicate: F |+| S, in: In) => {
    predicate.fold(PredicateChecker[In, F].test(_, in), PredicateChecker[In, S].test(_, in))
  }
  
  object syntax {
    implicit final class LiftOps[A](private val a: A) extends AnyVal {
      def first[S]: A |+| S = First(a)

      def second[F]: F |+| A = Second(a)
    }

    implicit final class DecoderOps[F](private val first: Decoder[F]) extends AnyVal {
      def |+|[S] (second: Decoder[S]): Decoder[F |+| S] = anyOf[F |+| S](
        first.map(First[F, S]).widen,
        second.map(Second[F, S]).widen
      )
    }

    implicit final class EncoderOps[F](private val first: Encoder[F]) extends AnyVal {
      def |+|[S](second: Encoder[S]): Encoder[F |+| S] = Encoder.instance[F |+| S](_.fold(first(_), second(_)))
    }

    implicit final class CheckerOps[I, F](private val first: PredicateChecker[I, F]) extends AnyVal {
      def |+|[S](second: PredicateChecker[I, S]): PredicateChecker[I, F |+| S] =
        (predicate: F |+| S, in: I) => predicate.fold(first.test(_, in), second.test(_, in))
    }
  }
}
