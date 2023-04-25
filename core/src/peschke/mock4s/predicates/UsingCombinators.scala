package peschke.mock4s.predicates

import cats.syntax.all._
import cats.{Defer, Eq}
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}
import peschke.mock4s.algebras.PredicateChecker
import peschke.mock4s.utils.Circe._
import peschke.mock4s.utils.Orphans._

sealed trait UsingCombinators[PA] {
  def upcast: UsingCombinators[PA] = this
}

object UsingCombinators {
  final case class Wrapped[PA](predicate: PA) extends UsingCombinators[PA]

  final case class Not[PA](combinator: UsingCombinators[PA]) extends UsingCombinators[PA]

  final case class ForAll[PA](combinators: List[UsingCombinators[PA]])
    extends UsingCombinators[PA]

  final case class Exists[PA](combinators: List[UsingCombinators[PA]])
    extends UsingCombinators[PA]

  implicit def eq[PA : Eq]: Eq[UsingCombinators[PA]] =
    Defer[Eq].fix { implicit eq =>
      Eq.instance {
        case (Wrapped(a), Wrapped(b)) => a === b
        case (Not(a), Not(b)) => a === b
        case (ForAll(a), ForAll(b)) => a === b
        case (Exists(a), Exists(b)) => a === b
        case _ => false
      }
    }

  implicit def decoder[PA: Decoder]: Decoder[UsingCombinators[PA]] =
    Defer[Decoder].fix[UsingCombinators[PA]] { implicit duc =>
      val ducL = Decoder[List[UsingCombinators[PA]]]
      anyOf(
        duc.map(Not[PA]).at("!").widen,
        ducL.map(ForAll[PA]).at("forall").widen,
        ducL.map(Exists[PA]).at("exists").widen,
        Decoder[PA].map(Wrapped[PA])
      )
    }

  implicit def encoder[PA: Encoder]: Encoder[UsingCombinators[PA]] = {
    Defer[Encoder].fix[UsingCombinators[PA]] { implicit euc =>
      Encoder.instance[UsingCombinators[PA]] {
        case Wrapped(predicate) => predicate.asJson
        case Not(combinator) => Json.obj("!" := combinator)
        case ForAll(combinators) => Json.obj("forall" := combinators)
        case Exists(combinators) => Json.obj("exists" := combinators)
      }
    }
  }

  implicit def create[In, PA](implicit checker: PredicateChecker[In, PA]): PredicateChecker[In, UsingCombinators[PA]] = {
    Defer[PredicateChecker[In, *]].fix { recurse =>
      (predicate: UsingCombinators[PA], in: In) => predicate match {
        case UsingCombinators.Wrapped(predicate) => checker.test(predicate, in)
        case UsingCombinators.Not(combinator) => !recurse.test(combinator, in)
        case UsingCombinators.ForAll(combinators) => combinators.forall(recurse.test(_, in))
        case UsingCombinators.Exists(combinators) => combinators.exists(recurse.test(_, in))
      }
    }
  }
}
