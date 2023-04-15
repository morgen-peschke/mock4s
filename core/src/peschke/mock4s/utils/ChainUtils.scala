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

  implicit final class Mock4sChainOps[V](private val initial: Chain[V]) extends AnyVal {
    def updateFirstBy[I: Eq](id: I)(fn: V => V)(implicit ID: Identify[V, I]): Option[Chain[V]] =
      updateFirstByE[I, Nothing](id: I)(v => fn(v).asRight).flatMap(_.toOption)

    def updateFirstByE[I: Eq, E](id: I)(fn: V => Either[E, V])(implicit ID: Identify[V, I]): Option[Either[E, Chain[V]]] = {
      @tailrec
      def loop(accum: Chain[V], pending: Chain[V]): Option[Either[E, Chain[V]]] =
        pending.uncons match {
          case None => none
          case Some(current -> rest) =>
            if (ID.id(current) === id) fn(current).map(accum.append(_).concat(rest)).some
            else loop(accum.append(current), rest)
        }

      loop(Chain.empty, initial)
    }

    def insertAfterBy[I: Eq](value: V, after: I)(implicit ID: Identify[V, I]): Option[Chain[V]] = {
      @tailrec
      def loop(accum: Chain[V], pending: Chain[V]): Option[Chain[V]] =
        pending.uncons match {
          case None => none
          case Some(current -> rest) =>
            if (ID.id(current) === after)
              accum.append(current).append(value).concat(rest).some
            else
              loop(accum.append(current), rest)
        }

      loop(Chain.empty, initial)
    }

    def insertBeforeBy[I: Eq](value: V, before: I)(implicit ID: Identify[V, I]): Option[Chain[V]] = {
      @tailrec
      def loop(accum: Chain[V], pending: Chain[V]): Option[Chain[V]] =
        pending.uncons match {
          case None => none
          case Some(current -> rest) =>
            if (ID.id(current) === before)
              accum.append(value).append(current).concat(rest).some
            else
              loop(accum.append(current), rest)
        }

      loop(Chain.empty, initial)
    }

    def startsWith(prefix: Chain[V])(implicit eq: Eq[V]): Boolean = {
      val chainIter = initial.iterator
      val prefixIter = prefix.iterator

      @tailrec
      def loop(): Boolean =
        if (chainIter.hasNext && prefixIter.hasNext) {
          if (chainIter.next() =!= prefixIter.next()) false
          else loop()
        } else !prefixIter.hasNext

      loop()
    }

    def endsWith(suffix: Chain[V])(implicit eq: Eq[V]): Boolean =
      if (suffix.isEmpty) true
      else {
        val chainIter = initial.iterator.drop((initial.length - suffix.length).toInt)
        val suffixIter = suffix.iterator

        @tailrec
        def loop(): Boolean =
          if (chainIter.hasNext && suffixIter.hasNext) {
            if (chainIter.next() =!= suffixIter.next()) false
            else loop()
          } else !suffixIter.hasNext

        loop()
      }

    def containsEq(subChain: Chain[V])(implicit eq: Eq[V]): Boolean =
        subChain.uncons match {
          case None => true
          case Some(marker -> restOfSubChain) =>
            if (initial.isEmpty) false
            else if (subChain.lengthCompare(initial.length) > 0) false
            else {
              @tailrec
              def checkForMatch(matchPoints: Chain[PotentialSubChain[V]]): Either[Boolean, Iterator[V]] =
                matchPoints.uncons match {
                  case None => false.asLeft// End of initialChain reached
                  case Some(state -> rest) =>
                    val chainHasNext = state.chain.hasNext
                    val subChainHasNext = state.subChain.hasNext
                    if (chainHasNext && subChainHasNext) {
                      val cValue = state.chain.next()
                      val sValue = state.subChain.next()
                      if (cValue =!= sValue && cValue =!= marker) {
                        // chainItr didn't match, and we didn't find another marker, so continue searching other match points
                        checkForMatch(rest)
                      } else if (cValue =!= sValue && cValue === marker) {
                        // chainIter didn't match, but we found another marker, so add that to the search space
                        val updatedState = PotentialSubChain(state.chain, restOfSubChain.iterator)
                        checkForMatch(rest.append(updatedState))
                      }
                      else if (cValue === sValue && cValue =!= marker) {
                        // chainItr is still a potential match, but we didn't find another marker, so no branching needed
                        checkForMatch(matchPoints)
                      } else {
                        // chainItr is still a potential match, and we found another marker, so we need to branch
                        val (updatedChainItr, newMarkerIter) = state.chain.duplicate
                        checkForMatch(
                          rest
                            .prepend(state.copy(chain = updatedChainItr))
                            .append(PotentialSubChain(newMarkerIter, restOfSubChain.iterator))
                        )
                      }
                    }
                    else if (!subChainHasNext) true.asLeft
                    else {
                      if (rest.isEmpty) state.chain.asRight
                      else checkForMatch(rest)
                    }
                }

              @tailrec
              def findNextMarker(iterator: Iterator[V]): Boolean =
                if (iterator.hasNext) {
                  if (iterator.next() =!= marker) findNextMarker(iterator)
                  else {
                    checkForMatch(Chain.one(PotentialSubChain(iterator, restOfSubChain.iterator))) match {
                      case Left(result) => result
                      case Right(nextIterator) => findNextMarker(nextIterator)
                    }
                  }
                } else false

              findNextMarker(initial.iterator)
            }
        }
  }
  private final case class PotentialSubChain[V](chain: Iterator[V], subChain: Iterator[V])
}
