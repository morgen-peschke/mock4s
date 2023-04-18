package peschke.mock4s.algebras

import cats.Eq
import cats.data.Chain
import cats.effect.{Ref, Sync}
import cats.syntax.all._
import peschke.mock4s.utils.ChainUtils._

trait DataStore[F[_], K, V] {
  def entries: F[Chain[(K, Chain[V])]]

  def get(key: K): F[Option[Chain[V]]]
  def getOrEmpty(key: K): F[Chain[V]]

  def replace(key: K, values: Chain[V]): F[Unit]
  def clear(key: K): F[Unit]

  def push(key: K, value: V): F[Unit]
  def unshift(key: K, value: V): F[Unit]

  def pop(key: K): F[Option[V]]
  def shift(key: K): F[Option[V]]

  def updateE[E](key: K)(fn: Chain[V] => Either[E,Chain[V]]): F[Option[Either[E, Unit]]]

  def remove(key: K, value: V)(implicit eq: Eq[V]): F[Unit]
  def removeBy[I: Eq](key: K, id: I)(implicit ID: Identify[V, I]): F[Unit]

  def insertAfterBy[I: Eq](key: K, value: V, after: I)(implicit ID: Identify[V,I]): F[Boolean]
  def insertBeforeBy[I: Eq](key: K, value: V, before: I)(implicit ID: Identify[V,I]): F[Boolean]
}

object DataStore {
  def empty[F[_] : Sync, K, V]: F[DataStore[F, K, V]] = init[F,K,V](Chain.empty)

  def init[F[_]: Sync, K, V](values: Chain[(K, Chain[V])]): F[DataStore[F, K, V]] =
    Ref[F].of(values.toVector.toMap).map { mapRef =>
      new DataStore[F, K, V] {
        override def entries: F[Chain[(K, Chain[V])]] = mapRef.get.map { entriesMap =>
          Chain.fromSeq(entriesMap.toSeq)
        }

        override def get(key: K): F[Option[Chain[V]]] = mapRef.get.map(_.get(key))

        override def getOrEmpty(key: K): F[Chain[V]] = get(key).map(_.orEmpty)

        override def replace(key: K, values: Chain[V]): F[Unit] = mapRef.update(_.updated(key, values))

        override def clear(key: K): F[Unit] = mapRef.update(_.removed(key))

        override def push(key: K, value: V): F[Unit] = mapRef.update(_.updatedWith[Chain[V]](key) {
          _.fold(Chain.one(value))(_.prepend(value)).some
        })

        override def unshift(key: K, value: V): F[Unit] = mapRef.update(_.updatedWith[Chain[V]](key) {
          _.fold(Chain.one(value))(_.append(value)).some
        })

        override def pop(key: K): F[Option[V]] = mapRef.modify { map =>
          map.get(key) match {
            case None => map -> none[V]
            case Some(values) =>
              values.uncons match {
                case None => map.removed(key) -> none[V]
                case Some(value -> tail) => map.updated(key, tail) -> value.some
              }
          }
        }

        override def shift(key: K): F[Option[V]] = mapRef.modify { map =>
          map.get(key) match {
            case None => map -> none[V]
            case Some(values) =>
              values.initLast match {
                case None => map.removed(key) -> none[V]
                case Some(init -> value) => map.updated(key, init) -> value.some
              }
          }
        }

        override def updateE[E](key: K)(fn: Chain[V] => Either[E, Chain[V]]): F[Option[Either[E, Unit]]] =
          mapRef.modify { map =>
            map.get(key).map(fn(_))
              .fold(map -> none[Either[E, Unit]])(_.fold(
                map -> _.asLeft.some,
                map.updated(key, _) -> ().asRight.some
              ))
          }

        override def remove(key: K, value: V)(implicit eq: Eq[V]): F[Unit] = removeBy[V](key, value)

        override def removeBy[I: Eq](key: K, id: I)(implicit ID: Identify[V, I]): F[Unit] =
          mapRef.update { map =>
            map.get(key)
              .flatMap(_.deleteFirst(ID.id(_) === id))
              .fold(map)(updated => map.updated(key, updated._2))
          }

        override def insertAfterBy[I: Eq](key: K, value: V, after: I)(implicit ID: Identify[V, I]): F[Boolean] =
          mapRef.modify { map =>
            map.get(key)
              .flatMap(_.insertAfterBy(value, after))
              .fold(map -> false)(updated => map.updated(key, updated) -> true)
          }

        override def insertBeforeBy[I: Eq](key: K, value: V, before: I)(implicit ID: Identify[V, I]): F[Boolean] =
          mapRef.modify { map =>
            map.get(key)
              .flatMap(_.insertAfterBy(value, before))
              .fold(map -> false)(updated => map.updated(key, updated) -> true)
          }
      }
    }
}
