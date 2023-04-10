package peschke.mock4s.predicates

import cats.Eq
import cats.syntax.all._
import io.circe.syntax._
import io.circe.{Decoder, Encoder}
import peschke.mock4s.predicates.Predicate.UsingCombinators
import peschke.mock4s.utils.Circe._

trait PredicateWrapper[T] { self =>
  type |+|[L <: Predicate[T], R <: Predicate[T]] = PredicateWrapper.Or[T, L, R]
  val |+| = PredicateWrapper.Or

  type Base <: Predicate[T]
  type Combined = Base |+| UsingCombinators[T, Base]
  type Type = Combined with self.type

  implicit def baseDecoder: Decoder[Base]
  implicit def baseEncoder: Encoder[Base]

  def combinedDecoder: Decoder[Combined] = GeneratedDecoder[Combined].decoder
  def combinedEncoder: Encoder[Combined] = GeneratedEncoder[Combined].encoder

  def wrap(c: Combined): Type = c.asInstanceOf[Type]
  def unwrap(ct: Type): Combined = ct

  def lhs[L <: Predicate[T], R <: Predicate[T]](l: L): L |+| R = PredicateWrapper.Lhs(l)
  def rhs[L <: Predicate[T], R <: Predicate[T]](r: R): L |+| R = PredicateWrapper.Rhs(r)

  def liftToCombinators(c: Combined): UsingCombinators[T, Base] =
    c.fold(UsingCombinators.Wrapped(_), identity)

  def not(p: Type): Type = wrap {
    rhs[Base, UsingCombinators[T, Base]](UsingCombinators.Not[T, Base](liftToCombinators(p)))
  }

  def forall(px: List[Type]): Type = wrap {
    rhs[Base, UsingCombinators[T, Base]](UsingCombinators.ForAll[T, Base](px.map(liftToCombinators)))
  }

  def exists(px: List[Type]): Type = wrap {
    rhs[Base, UsingCombinators[T, Base]](UsingCombinators.Exists[T, Base](px.map(liftToCombinators)))
  }

  implicit lazy val decoder: Decoder[Type] = combinedDecoder.map(wrap)
  implicit lazy val encoder: Encoder[Type] = combinedEncoder.contramap(unwrap)

  implicit def eq(implicit C: Eq[Combined]): Eq[Type] = Eq.by(unwrap)
}
object PredicateWrapper   {
  sealed abstract class Or[T, L <: Predicate[T], R <: Predicate[T]] extends Predicate[T] {
    def fold[A](lf: L => A, rf: R => A): A
  }

  trait GeneratedFromNoProvidedInstances {
    implicit def fallbackDecoder[
        T,
        L <: Predicate[T]: GeneratedDecoder,
        R <: Predicate[T]: GeneratedDecoder
    ]: GeneratedDecoder[Or[T, L, R]] = Or.mergeTwoDecoders(GeneratedDecoder[L].decoder, GeneratedDecoder[R].decoder)

    implicit def fallbackEncoder[
        T,
        L <: Predicate[T]: GeneratedEncoder,
        R <: Predicate[T]: GeneratedEncoder
    ]: GeneratedEncoder[Or[T, L, R]] = Or.mergeTwoEncoders(GeneratedEncoder[L].encoder, GeneratedEncoder[R].encoder)
  }

  trait GeneratedFromProvidedLeftInstances extends GeneratedFromNoProvidedInstances {
    implicit def rightOnlyDecoder[
        T,
        L <: Predicate[T]: Decoder,
        R <: Predicate[T]: GeneratedDecoder
    ]: GeneratedDecoder[Or[T, L, R]] = Or.mergeTwoDecoders(Decoder[L], GeneratedDecoder[R].decoder)

    implicit def leftOnlyEncoder[
        T,
        L <: Predicate[T]: Encoder,
        R <: Predicate[T]: GeneratedEncoder
    ]: GeneratedEncoder[Or[T, L, R]] = Or.mergeTwoEncoders(Encoder[L], GeneratedEncoder[R].encoder)
  }

  trait GeneratedFromProvidedRightInstance extends GeneratedFromProvidedLeftInstances {
    implicit def leftOnlyDecoder[
        T,
        L <: Predicate[T]: GeneratedDecoder,
        R <: Predicate[T]: Decoder
    ]: GeneratedDecoder[Or[T, L, R]] = Or.mergeTwoDecoders(GeneratedDecoder[L].decoder, Decoder[R])

    implicit def leftOnlyEncoder[
        T,
        L <: Predicate[T]: GeneratedEncoder,
        R <: Predicate[T]: Encoder
    ]: GeneratedEncoder[Or[T, L, R]] = Or.mergeTwoEncoders(GeneratedEncoder[L].encoder, Encoder[R])
  }

  trait GeneratedFromTwoProvidedInstances extends GeneratedFromProvidedRightInstance {
    implicit def mergeTwoDecoders[
        T,
        L <: Predicate[T]: Decoder,
        R <: Predicate[T]: Decoder
    ]: GeneratedDecoder[Or[T, L, R]] = GeneratedDecoder(
      anyOf[Or[T, L, R]](
        Decoder[L].map(Lhs[T, L, R]).widen,
        Decoder[R].map(Rhs[T, L, R]).widen
      )
    )

    implicit def mergeTwoEncoders[
        T,
        L <: Predicate[T]: Encoder,
        R <: Predicate[T]: Encoder
    ]: GeneratedEncoder[Or[T, L, R]] = GeneratedEncoder(Encoder.instance[Or[T, L, R]](_.fold(_.asJson, _.asJson)))
  }

  object Or extends GeneratedFromTwoProvidedInstances {
    implicit def eq[T, L <: Predicate[T]: Eq, R <: Predicate[T]: Eq]: Eq[Or[T, L, R]] = Eq.instance {
      case (Lhs(a), Lhs(b)) => a === b
      case (Rhs(a), Rhs(b)) => a === b
      case _                => false
    }
  }

  final case class Lhs[T, L <: Predicate[T], R <: Predicate[T]](l: L) extends Or[T, L, R] {
    override def test(t: T): Boolean = l.test(t)

    override def fold[A](lf: L => A, rf: R => A): A = lf(l)
  }

  final case class Rhs[T, L <: Predicate[T], R <: Predicate[T]](r: R) extends Or[T, L, R] {
    override def test(t: T): Boolean = r.test(t)

    override def fold[A](lf: L => A, rf: R => A): A = rf(r)
  }
}
