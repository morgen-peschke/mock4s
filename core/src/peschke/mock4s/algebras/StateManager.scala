package peschke.mock4s.algebras

import cats.Show
import cats.effect.Ref
import cats.effect.kernel.Sync
import cats.syntax.all._
import io.circe.Json
import peschke.mock4s.models.MockDefinition.{ActionName, MockName}
import peschke.mock4s.models.MockState

trait StateManager[F[_]] {
  def retrieve: F[MockState.State]

  def get(key: MockState.Key): F[Option[Json]]

  def update(key: MockState.Key, value: Json): F[Unit]

  def clear(key: MockState.Key): F[Unit]

  def reset: F[Unit]
}

object StateManager {
  def apply[F[_]](implicit SM: StateManager[F]): SM.type = SM

  sealed trait ManagerError
  object ManagerError {
    final case class MockNotFound(mockName: MockName) extends ManagerError
    final case class ActionNotFound(mockName: MockName, actionName: ActionName) extends ManagerError
    final case class DuplicateMockFound(mockName: MockName) extends ManagerError
    final case class DuplicateActionFound(mockName: MockName, actionName: ActionName) extends ManagerError

    implicit val show: Show[ManagerError] = Show.fromToString
  }

  def initialize[F[_]: Sync](initialState: MockState.State): F[StateManager[F]] =
    Ref[F]
      .of(initialState)
      .map { stateData =>
        new StateManager[F] {
          override def retrieve: F[MockState.State] = stateData.get

          def get(key: MockState.Key): F[Option[Json]] = stateData.get.map { state =>
            MockState.State.raw(state).get(key)
          }

          override def update(key: MockState.Key, value: Json): F[Unit] =
            stateData.update { state =>
              MockState.State(MockState.State.raw(state).updated(key, value))
            }

          override def clear(key: MockState.Key): F[Unit] =
            stateData.update { state =>
              MockState.State(MockState.State.raw(state).removed(key))
            }

          override def reset: F[Unit] = stateData.set(MockState.State(Map.empty[MockState.Key, Json]))
        }
      }
}