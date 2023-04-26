package peschke.mock4s

import cats.effect.kernel.Concurrent
import cats.syntax.all._
import io.circe.syntax._
import io.circe.{Decoder, Json}
import org.http4s._
import org.http4s.circe.CirceEntityDecoder.circeEntityDecoder
import org.http4s.circe.CirceEntityEncoder.circeEntityEncoder
import org.http4s.dsl.Http4sDsl
import peschke.mock4s.algebras.{MocksManager, StateManager}
import peschke.mock4s.algebras.MocksManager.{InsertLocation, ManagerError}
import peschke.mock4s.models.{MockDefinition, MockState}
import peschke.mock4s.models.MockDefinition.{Action, ActionName, MockName}
import peschke.mock4s.utils.Circe._

object AdminRoutes {

  final case class AddMock(mock: MockDefinition, at: InsertLocation[MockName])
  final case class AddAction(action: Action, at: InsertLocation[ActionName])

  implicit val addMockDecoder: Decoder[AddMock] = accumulatingDecoder { c =>
    (
      c.downField("mock").asAcc[MockDefinition],
      c.downField("at").asAcc[InsertLocation[MockName]]
    ).mapN(AddMock)
  }

  implicit val addActionDecoder: Decoder[AddAction] = accumulatingDecoder { c =>
    (
      c.downField("action").asAcc[Action],
      c.downField("at").asAcc[InsertLocation[ActionName]]
    ).mapN(AddAction)
  }


  def default[F[_]: Concurrent: MocksManager: StateManager](endpointRoot: Uri.Path): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F] {}
    import dsl._
    val RootPath = endpointRoot

    def managerErrorResponse(error: ManagerError): F[Response[F]] =
      error match {
        case ManagerError.MockNotFound(mockName) =>
          NotFound(Json.obj(
            "error" := "No mock found with that name",
            "details" := Json.obj("mock" := mockName)
          ))
        case ManagerError.ActionNotFound(mockName, actionName) =>
          NotFound(Json.obj(
            "error" := "No action found with that name",
            "details" := Json.obj("mock" := mockName, "action" := actionName)
          ))
        case ManagerError.DuplicateMockFound(mockName) =>
          BadRequest(Json.obj(
            "error" := "Mock with this name already exists",
            "details" := Json.obj("mock" := mockName)
          ))
        case ManagerError.DuplicateActionFound(mockName, actionName) =>
          BadRequest(Json.obj(
            "error" := "Action with this name already exists",
            "details" := Json.obj("mock" := mockName, "action" := actionName)
          ))
      }

    HttpRoutes.of[F] {
      case GET -> RootPath / "mocks" =>
        MocksManager[F].listAllMocks.flatMap(Ok(_))

      case GET -> RootPath / "mocks" / MockName(mockName) =>
        MocksManager[F].retrieveMock(mockName).flatMap(_.fold(managerErrorResponse, Ok(_)))

      case GET -> RootPath / "mocks" / MockName(mockName) / "actions" =>
        MocksManager[F].listMockActions(mockName).flatMap(_.fold(managerErrorResponse, Ok(_)))

      case GET -> RootPath / "mocks" / MockName(mockName) / "actions" / ActionName(actionName) =>
        MocksManager[F].retrieveAction(mockName, actionName).flatMap(_.fold(managerErrorResponse, Ok(_)))

      case req @ POST -> RootPath / "mocks" =>
        req.as[AddMock].flatMap { addMock =>
          MocksManager[F]
            .addMock(addMock.mock, addMock.at)
            .flatMap(_.fold(managerErrorResponse, _ => Created(Json.obj("created" := addMock.mock.name))))
        }

      case req@POST -> RootPath / "mocks" / MockName(mockName) / "actions" =>
        req.as[AddAction].flatMap { addAction =>
          MocksManager[F]
            .addAction(mockName, addAction.action, addAction.at)
            .flatMap(_.fold(managerErrorResponse, _ => Created(Json.obj("created" := addAction.action.name))))
        }

      case DELETE -> RootPath / "mocks" / MockName(mockName) =>
        MocksManager[F]
          .deleteMock(mockName)
          .flatMap(_ => Accepted(Json.obj("deleted" := mockName)))

      case DELETE -> RootPath / "mocks" / MockName(mockName) / "actions" / ActionName(actionName) =>
        MocksManager[F]
          .deleteAction(mockName, actionName)
          .flatMap(_ => Accepted(Json.obj("deleted" := actionName)))

      case GET -> RootPath / "state" / "all" => StateManager[F].retrieve.flatMap(Ok(_))

      case GET -> RootPath / "state" / "keys" / key =>
        StateManager[F].get(MockState.Key(key)).flatMap(Ok(_))

      case req @  POST -> RootPath / "state" / "keys" / key =>
        req.as[Json]
          .flatMap(StateManager[F].update(MockState.Key(key), _))
          .flatMap(_ => Accepted(Json.obj("updated" := key)))

      case DELETE -> RootPath / "state" / "keys" / key =>
        StateManager[F]
          .clear(MockState.Key(key))
          .flatMap(_ => Accepted(Json.obj("deleted" := key)))

      case DELETE -> RootPath / "state" / "keys" =>
        StateManager[F].clearAll.flatMap(_ => Accepted(Json.obj("deleted-all" := true)))

      case POST -> RootPath / "state" / "reset" =>
        StateManager[F].reset.flatMap(_ => Ok(Json.obj("reset" := true)))
    }
  }
}
