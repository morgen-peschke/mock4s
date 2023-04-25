package peschke.mock4s.algebras

import cats.Defer

import scala.annotation.tailrec

trait PredicateChecker[I, P] {
  def test(predicate: P, in: I): Boolean

  def contraMapPredicate[P0](f: P0 => P): PredicateChecker[I, P0] = {
    val self = this
    (predicate, in) => self.test(f(predicate), in)
  }
}
object PredicateChecker {
  def apply[In, ADT](implicit PC: PredicateChecker[In, ADT]): PC.type = PC

  object syntax {
    implicit final class PredicateCheckerOps[A](private val a: A) extends AnyVal {
      def satisfies[ADT](predicate: ADT)(implicit checker: PredicateChecker[A, ADT]): Boolean =
        checker.test(predicate, a)

      def satisfiedBy[In](in: In)(implicit checker: PredicateChecker[In, A]): Boolean =
        checker.test(a, in)
    }
  }

  private case class Deferred[In, ADT](fpc: () => PredicateChecker[In, ADT]) extends PredicateChecker[In, ADT] {
    private lazy val resolved: PredicateChecker[In, ADT] = resolve(fpc)

    @tailrec
    private def resolve(f: () => PredicateChecker[In, ADT]): PredicateChecker[In, ADT] =
      f() match {
        case Deferred(next) => resolve(next)
        case checker => checker
      }

    override def test(predicate: ADT, in: In): Boolean = resolved.test(predicate, in)
  }

  implicit def deferInstance[In]: Defer[PredicateChecker[In,*]] = new Defer[PredicateChecker[In, *]] {
    override def defer[A](fa: => PredicateChecker[In, A]): PredicateChecker[In, A] =
      Deferred[In, A](() => fa)
  }
}