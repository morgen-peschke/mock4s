package peschke.mock4s.algebras

import cats.Monad
import cats.data.Chain
import cats.syntax.all._
import io.circe.Json
import io.circe.syntax._
import org.http4s.Request
import org.http4s.Status
import org.typelevel.log4cats.LoggerFactory
import peschke.mock4s.algebras.PredicateChecker.syntax._
import peschke.mock4s.models.Body
import peschke.mock4s.models.MockDefinition.Action
import peschke.mock4s.models.MockDefinition.MockName
import peschke.mock4s.models.ParsedRequest
import peschke.mock4s.models.ResponseDef
import peschke.mock4s.utils.Circe._

trait RequestMatcher[F[_]] {
  def findResponse(request: Request[F]): F[ResponseDef]
}
object RequestMatcher      {
  def apply[F[_]](implicit RP: RequestMatcher[F]): RP.type = RP

  def default[F[_]: Monad: RequestParser: LoggerFactory: MocksManager]: RequestMatcher[F] =
    new RequestMatcher[F] {
      private val logger = LoggerFactory[F].getLogger

      private def findMockDefinition(parsedRequest: ParsedRequest): F[Option[MockName]] = {
        MocksManager[F].listAllRoutes.flatMap { mocks =>
          mocks.find(_._2.satisfiedBy(parsedRequest.route)) match {
            case None                => logger.warn("Unable to find matching mock definition").as(none[MockName])
            case Some((mockName, _)) =>
              logger.info(show"Matched mock definition: $mockName").as(mockName.some)
          }
        }
      }

      private def findAction(actions: Chain[Action], parsedRequest: ParsedRequest): F[Option[Action]] =
        actions.find(_.when.satisfiedBy(parsedRequest)) match {
          case a @ Some(action) => logger.info(show"Found action: ${action.name}").as(a)
          case None             => logger.warn("Unable to find matching action").as(none[Action])
        }

      private def notFound(request: Request[F], parsedRequest: ParsedRequest): ResponseDef =
        ResponseDef(
          Status.NotFound,
          request.httpVersion.some,
          None,
          Body.JsonBody(
            Json.obj(
              "error" := "Not Found",
              "request" -> Json.obj(
                "method"  := parsedRequest.route.method,
                "path"    := parsedRequest.route.path,
                "query"   := parsedRequest.route.query,
                "headers" := parsedRequest.headers,
                "body"    := parsedRequest.body
              )
            )
          ),
          None
        )

      override def findResponse(request: Request[F]): F[ResponseDef] =
        RequestParser[F]
          .parse(request)
          .flatTap { parsedRequest =>
            logger.info(s"Parsed request as: ${parsedRequest.asJson.noSpaces}")
          }
          .flatMap { parsedRequest =>
            findMockDefinition(parsedRequest)
              .flatMap(_.traverse(MocksManager[F].listMockActions(_)))
              .flatMap(_.flatTraverse {
                case Left(lookupFailed) =>
                  logger
                    .warn(show"Matched against a route for a mock that no longer exists: $lookupFailed")
                    .as(none[Action])
                case Right(actions)     => findAction(actions, parsedRequest)
              })
              .map(_.fold(notFound(request, parsedRequest))(_.respondWith))
          }
    }
}
