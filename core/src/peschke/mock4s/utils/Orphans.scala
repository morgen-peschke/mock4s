package peschke.mock4s.utils

import cats.{Defer, Eq, PartialOrder}

import scala.annotation.tailrec

object Orphans {
  case class DeferredEq[A](eq: () => Eq[A]) extends Eq[A] {
    override def eqv(x: A, y: A): Boolean = {
      @tailrec
      def loop(f: () => Eq[A]): Boolean =
        f() match {
          case DeferredEq(f) => loop(f)
          case next          => next.eqv(x, y)
        }

      loop(eq)
    }
  }
  implicit val eqDefer: Defer[Eq] = new Defer[Eq] {
    override def defer[A](fa: => Eq[A]): Eq[A] = DeferredEq(() => fa)
  }

  case class DeferredPartialOrder[A](PartialOrder: () => PartialOrder[A]) extends PartialOrder[A] {
    override def partialCompare(x: A, y: A): Double = {
      @tailrec
      def loop(f: () => PartialOrder[A]): Double =
        f() match {
          case DeferredPartialOrder(f) => loop(f)
          case next                    => next.partialCompare(x, y)
        }

      loop(PartialOrder)
    }
  }

  implicit val PartialOrderDefer: Defer[PartialOrder] = new Defer[PartialOrder] {
    override def defer[A](fa: => PartialOrder[A]): PartialOrder[A] = DeferredPartialOrder(() => fa)
  }
}
