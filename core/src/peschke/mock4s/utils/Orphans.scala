package peschke.mock4s.utils

import cats.{Defer, Eq}

import scala.annotation.tailrec

object Orphans {
  case class DeferredEq[A](eq: () => Eq[A]) extends Eq[A] {
    override def eqv(x: A, y: A): Boolean = {
      @tailrec
      def loop(f: () => Eq[A]): Boolean =
        f() match {
          case DeferredEq(f) => loop(f)
          case next => next.eqv(x,y)
        }

      loop(eq)
    }
  }
  implicit val eqDefer: Defer[Eq] = new Defer[Eq] {
    override def defer[A](fa: => Eq[A]): Eq[A] = DeferredEq(() => fa)
  }
}
