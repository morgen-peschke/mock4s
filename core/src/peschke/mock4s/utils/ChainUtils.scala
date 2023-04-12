package peschke.mock4s.utils

import cats.Eq
import cats.data.Chain
import cats.syntax.all._

import scala.annotation.tailrec

object ChainUtils {
  trait Identify[A, I] {
    def id(a: A): I
  }

  object Identify {
    def apply[A, I](implicit ID: Identify[A, I]): ID.type = ID

    implicit def selfIdentify[A]: Identify[A, A] = a => a
  }

  implicit final class ChainOps[V](private val initialChain: Chain[V]) extends AnyVal {
    def updateBy[I: Eq](id: I)(fn: V => V)(implicit ID: Identify[V,I]): Option[Chain[V]] =
      updateByE[I, Nothing](id: I)(v => fn(v).asRight).flatMap(_.toOption)

    def updateByE[I: Eq, E](id: I)(fn: V => Either[E, V])(implicit ID: Identify[V, I]): Option[Either[E, Chain[V]]] = {
      @tailrec
      def loop(accum: Chain[V], pending: Chain[V]): Option[Either[E, Chain[V]]] =
        pending.uncons match {
          case None => none
          case Some(current -> rest) =>
            if (ID.id(current) === id) fn(current).map(accum.append(_).concat(pending)).some
            else loop(accum.append(current), rest)
        }

      loop(Chain.empty, initialChain)
    }


    def insertAfterBy[I: Eq](value: V, after: I)(implicit ID: Identify[V, I]): Option[Chain[V]] = {
      @tailrec
      def loop(accum: Chain[V], pending: Chain[V]): Option[Chain[V]] =
        pending.uncons match {
          case None => none
          case Some(current -> rest) =>
            if (ID.id(current) === after)
              accum.append(current).append(value).concat(pending).some
            else
              loop(accum.append(current), rest)
        }

      loop(Chain.empty, initialChain)
    }

    def insertBeforeBy[I: Eq](value: V, before: I)(implicit ID: Identify[V, I]): Option[Chain[V]] = {
      @tailrec
      def loop(accum: Chain[V], pending: Chain[V]): Option[Chain[V]] =
        pending.uncons match {
          case None => none
          case Some(current -> rest) =>
            if (ID.id(current) === before)
              accum.append(value).append(current).concat(pending).some
            else
              loop(accum.append(current), rest)
        }

      loop(Chain.empty, initialChain)
    }
  }
}
