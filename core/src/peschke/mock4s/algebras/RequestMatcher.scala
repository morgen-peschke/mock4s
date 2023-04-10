package peschke.mock4s.algebras

import cats.Monad
import cats.syntax.all._
import io.circe.Json
import io.circe.syntax._
import org.http4s.{Request, Status}
import org.typelevel.log4cats.LoggerFactory
import peschke.mock4s.models.MockDefinition.Action
import peschke.mock4s.models.{Body, MockDefinition, ParsedRequest, ResponseDef}
import peschke.mock4s.utils.Circe._

trait RequestMatcher[F[_]] {
  def findResponse(request: Request[F]): F[ResponseDef]
}
object RequestMatcher      {
  def apply[F[_]](implicit RP: RequestMatcher[F]): RP.type = RP

  def default[F[_]: Monad: RequestParser: LoggerFactory](mocks: List[MockDefinition]): RequestMatcher[F] =
    new RequestMatcher[F] {
      private val logger = LoggerFactory[F].getLogger

      private def findMockDefinition(parsedRequest: ParsedRequest): F[Option[MockDefinition]] =
        mocks.find(_.route.test(parsedRequest.route)) match {
          case md @ Some(mockDefinition) =>
            logger.info(s"Matched mock definition: ${mockDefinition.name}").as(md)
          case None                      => logger.warn("Unable to find matching mock definition").as(none[MockDefinition])
        }

      private def findAction(actions: List[Action], parsedRequest: ParsedRequest): F[Option[Action]] =
        actions.find(_.when.test(parsedRequest)) match {
          case a @ Some(action) => logger.info(s"Found action: ${action.name}").as(a)
          case None             => logger.warn("Unable to find matching action").as(none[Action])
        }

      private def notFound(request: Request[F], parsedRequest: ParsedRequest): ResponseDef =
        ResponseDef(
          Status.NotFound,
          request.httpVersion,
          Nil,
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
          )
        )

      override def findResponse(request: Request[F]): F[ResponseDef] =
        RequestParser[F]
          .parse(request)
          .flatTap { parsedRequest =>
            logger.info(s"Parsed request as: ${parsedRequest.asJson.noSpaces}")
          }
          .flatMap { parsedRequest =>
            findMockDefinition(parsedRequest)
              .flatMap(_.flatTraverse(md => findAction(md.actions, parsedRequest)))
              .map(_.fold(notFound(request, parsedRequest))(_.respondWith))
          }
    }
}
