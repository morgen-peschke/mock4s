package peschke.mock4s.algebras

import cats.Show
import cats.data.Chain
import cats.effect.Ref
import cats.effect.kernel.Sync
import cats.syntax.all._
import io.circe.Decoder
import peschke.mock4s.algebras.MocksManager.ManagerError.{ActionNotFound, DuplicateActionFound, DuplicateMockFound, MockNotFound}
import peschke.mock4s.algebras.MocksManager.{InsertLocation, ManagerError}
import peschke.mock4s.models.MockDefinition
import peschke.mock4s.models.MockDefinition.{Action, ActionName, MockName}
import peschke.mock4s.predicates
import peschke.mock4s.predicates.RoutePredicate
import peschke.mock4s.utils.ChainUtils._
import peschke.mock4s.utils.Circe._

trait MocksManager[F[_]] {
  def listAllMocks: F[Chain[MockDefinition]]

  def listAllRoutes: F[Chain[(MockName, RoutePredicate.Type)]]

  def retrieveMock(mockName: MockName): F[Either[ManagerError, MockDefinition]]

  def listMockActions(mockName: MockName): F[Either[ManagerError, Chain[Action]]]

  def retrieveAction(mockName: MockName, actionName: ActionName): F[Either[ManagerError, Action]]

  def addMock(mock: MockDefinition, location: InsertLocation[MockName]): F[Either[ManagerError, Unit]]

  def deleteMock(mockName: MockName): F[Unit]

  def updateMock(mock: MockDefinition): F[Unit]

  def addAction(mockName: MockName, action: Action, location: InsertLocation[ActionName]): F[Either[ManagerError, Unit]]

  def deleteAction(mockName: MockName, actionName: ActionName): F[Either[ManagerError, Unit]]

  def updateAction(mockName: MockName, action: Action): F[Either[ManagerError, Unit]]
}

object MocksManager {
  def apply[F[_]](implicit MM: MocksManager[F]): MM.type = MM

  sealed trait InsertLocation[Anchor]
  object InsertLocation {
    final case class Start[Anchor]() extends InsertLocation[Anchor]
    final case class End[Anchor]() extends InsertLocation[Anchor]
    final case class After[Anchor](anchor: Anchor) extends InsertLocation[Anchor]
    final case class Before[Anchor](anchor: Anchor) extends InsertLocation[Anchor]

    implicit def decoder[Anchor: Decoder]: Decoder[InsertLocation[Anchor]] = anyOf(
      fixed("start").as(Start()),
      fixed("end").as(End()),
      Decoder[Anchor].at("after").map(After[Anchor]),
      Decoder[Anchor].at("before").map(Before[Anchor])
    )
  }

  sealed trait ManagerError
  object ManagerError {
    final case class MockNotFound(mockName: MockName) extends ManagerError
    final case class ActionNotFound(mockName: MockName, actionName: ActionName) extends ManagerError
    final case class DuplicateMockFound(mockName: MockName) extends ManagerError
    final case class DuplicateActionFound(mockName: MockName, actionName: ActionName) extends ManagerError

    implicit val show: Show[ManagerError] = Show.fromToString
  }

  def initialize[F[_]: Sync](initialMocks: Chain[MockDefinition]): F[MocksManager[F]] = {
    val routesDataF = Ref[F].of(initialMocks.map(m => m.name -> m.route))
    val actionsDataF = DataStore.init[F, MockName, Action](
      initialMocks.map(m => m.name -> m.actions)
    )

    (routesDataF, actionsDataF).mapN { (routesData, actionsData) =>
      new MocksManager[F] {
        private implicit val identifyRoutesByMockName: Identify[(MockName, RoutePredicate.Type), MockName] = _._1
        private implicit val identifyActionsByActionName: Identify[Action, ActionName] = _.name

        private def retrieveFullMockDefinition(mock: (MockName, RoutePredicate.Type)): F[MockDefinition] = mock match {
          case (mockName, routePredicate) =>
            actionsData.getOrEmpty(mockName).map(MockDefinition(mockName, routePredicate, _))
        }

        override def listAllMocks: F[Chain[MockDefinition]] =
          routesData.get.flatMap(_.traverse(retrieveFullMockDefinition))

        override def listAllRoutes: F[Chain[(MockName, predicates.RoutePredicate.Type)]] = routesData.get

        override def retrieveMock(mockName: MockName): F[Either[ManagerError, MockDefinition]] =
          routesData.get
            .map(_.find(_._1 === mockName))
            .flatMap(_.traverse(retrieveFullMockDefinition))
            .map(_.toRight(MockNotFound(mockName)))

        override def listMockActions(mockName: MockName): F[Either[ManagerError, Chain[Action]]] =
          actionsData.get(mockName).map(_.toRight(MockNotFound(mockName)))

        override def retrieveAction(mockName: MockName, actionName: ActionName): F[Either[ManagerError, Action]] =
          listMockActions(mockName).map {
            _.flatMap(_.find(_.name === actionName).toRight[ManagerError](ActionNotFound(mockName, actionName)))
          }

        override def addMock(mock: MockDefinition, location: InsertLocation[MockName]): F[Either[ManagerError, Unit]] = {
          val routeToInsert = mock.name -> mock.route
          val updateRoutes: F[Either[ManagerError, Unit]] =
            routesData.modify { data =>
              if (data.exists(_._1 === mock.name)) data -> DuplicateMockFound(mock.name).asLeft.widen
              else
                location match {
                case InsertLocation.Start() => data.prepend(routeToInsert) -> ().asRight
                case InsertLocation.End() => data.append(routeToInsert) -> ().asRight
                case InsertLocation.After(anchor) =>
                  data.insertAfterBy(routeToInsert, anchor) match {
                    case Some(updated) => updated -> ().asRight
                    case None => data -> MockNotFound(anchor).asLeft
                  }
                case InsertLocation.Before(anchor) =>
                  data.insertBeforeBy(routeToInsert, anchor) match {
                    case Some(updated) => updated -> ().asRight
                    case None => data -> MockNotFound(anchor).asLeft
                  }
              }
            }

          updateRoutes.flatMap(_.traverse(_ => actionsData.replace(mock.name, mock.actions)))
        }

        override def deleteMock(mockName: MockName): F[Unit] =
          actionsData.clear(mockName) >>
            routesData.update { routes =>
              routes.deleteFirst(_._1 === mockName).fold(routes)(_._2)
            }

        override def updateMock(mock: MockDefinition): F[Unit] =
          actionsData.replace(mock.name, mock.actions) >>
          routesData.update { routes =>
            routes.updateFirstBy(mock.name)(_ => mock.name -> mock.route).getOrElse(routes)
          }

        override def addAction(mockName: MockName,
                               action: Action,
                               location: InsertLocation[ActionName]): F[Either[ManagerError, Unit]] =
          actionsData
            .updateE[ManagerError](mockName) { actions =>
              if (actions.exists(_.name === action.name)) DuplicateActionFound(mockName, action.name).asLeft
              else location match {
                case InsertLocation.Start() => actions.prepend(action).asRight
                case InsertLocation.End() => actions.append(action).asRight
                case InsertLocation.After(anchor) =>
                  actions.insertAfterBy(action, anchor).toRight(ActionNotFound(mockName, anchor))
                case InsertLocation.Before(anchor) =>
                  actions.insertBeforeBy(action, anchor).toRight(ActionNotFound(mockName, anchor))
              }
            }
            .map(_.getOrElse(MockNotFound(mockName).asLeft))


        override def deleteAction(mockName: MockName, actionName: ActionName): F[Either[ManagerError, Unit]] =
          actionsData
            .updateE[ManagerError](mockName) { actions =>
              actions
                .deleteFirst(_.name === actionName)
                .fold(actions)(_._2)
                .asRight
            }
            .map(_.getOrElse(MockNotFound(mockName).asLeft))

        override def updateAction(mockName: MockName, action: Action): F[Either[ManagerError, Unit]] =
          actionsData
            .updateE[ManagerError](mockName) {
              _.updateFirstBy(action.name)(_ => action).toRight(ActionNotFound(mockName, action.name))
            }
            .map(_.getOrElse(MockNotFound(mockName).asLeft))
      }
    }
  }
}