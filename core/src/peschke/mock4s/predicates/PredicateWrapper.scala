package peschke.mock4s.predicates

import cats.syntax.all._
import cats.{Eq, Show}
import io.circe.{Decoder, Encoder}
import peschke.mock4s.algebras.PredicateChecker
import peschke.mock4s.models.|+|
import peschke.mock4s.models.|+|.syntax.LiftOps

abstract class PredicateWrapper[T, B: Decoder: Encoder](implicit baseChecker: PredicateChecker[T, B])
  extends supertagged.NewType[B |+| UsingCombinators[B]] {
  type Base = B

  def wrap(c: Base |+| UsingCombinators[Base]): Type = apply(c)

  private def liftToCombinators(c: Base |+| UsingCombinators[Base]): UsingCombinators[Base] =
    c.fold(UsingCombinators.Wrapped(_), identity)

  def not(p: Type): Type = wrap(UsingCombinators.Not[Base](liftToCombinators(raw(p))).upcast.second[Base])

  def forall(px: List[Type]): Type =
    wrap(UsingCombinators.ForAll[Base](px.map(raw).map(liftToCombinators)).upcast.second[Base])

  def exists(px: List[Type]): Type =
    wrap(UsingCombinators.Exists[Base](px.map(raw).map(liftToCombinators)).upcast.second[Base])

  implicit lazy val decoder: Decoder[Type] = Decoder[Base |+| UsingCombinators[Base]].map(apply(_))
  implicit lazy val encoder: Encoder[Type] = Encoder[Base |+| UsingCombinators[Base]].contramap(raw)
  implicit lazy val checker: PredicateChecker[T, Type] =
    PredicateChecker[T, Base |+| UsingCombinators[Base]].contraMapPredicate(raw)

  implicit def eq(implicit C: Eq[Base |+| UsingCombinators[Base]]): Eq[Type] = Eq.by(raw)
  implicit def show(implicit C: Show[Base |+| UsingCombinators[Base]]): Show[Type] = Show.show(raw(_).show)
}
