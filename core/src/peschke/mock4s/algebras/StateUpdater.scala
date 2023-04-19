package peschke.mock4s.algebras

import cats.Monad
import cats.syntax.all._
import org.typelevel.log4cats.LoggerFactory
import peschke.mock4s.models.StateTransition

trait StateUpdater[F[_]] {
  def transition(st: StateTransition): F[Unit]

  def run(transitions: List[StateTransition]): F[Unit]
}
object StateUpdater {
  def apply[F[_]](implicit SU: StateUpdater[F]): SU.type = SU

  def default[F[_]: Monad: StateManager: LoggerFactory]: StateUpdater[F] = new StateUpdater[F] {
    private val logger = LoggerFactory[F].getLogger

    override def transition(st: StateTransition): F[Unit] =
      st match {
        case StateTransition.Clear(keys) =>
          keys.traverse(StateManager[F].clear) >> logger.info(show"State cleared: $keys")
        case StateTransition.Set(entries) =>
          entries.foldMapM {
            case (key, value) =>
              StateManager[F].update(key, value) >> logger.info(show"State $key updated to $value")
          }
      }

    override def run(transitions: List[StateTransition]): F[Unit] =
      transitions.traverse(transition).void
  }
}
