package peschke.mock4s.algebras

import cats.effect.kernel.Concurrent
import cats.syntax.all._
import io.circe.Json
import io.circe.syntax._
import org.http4s.{Request, Status}
import peschke.mock4s.models.{Body, MockDefinition, ResponseDef}
import peschke.mock4s.utils.Circe._

trait RequestMatcher[F[_]] {
  def findResponse(request: Request[F]): F[ResponseDef]
}
object RequestMatcher {
  def default[F[_]: Concurrent](
                                 mocks: List[MockDefinition],
                                 bodyParser: BodyParser[F]
                               ): RequestMatcher[F] =
    request => bodyParser.parse(request).map { body =>

      def routeMatches(route: MockDefinition.RouteDef): Boolean =
        route.method.test(request.method) && route.path.test(request.uri.path)

      def actionMatches(action: MockDefinition.Action): Boolean =
        action.when.method.test(request.method) &&
          action.when.path.test(request.uri.path) &&
          action.when.query.test(request.uri.query) &&
          action.when.headers.forall { hpred =>
            request.headers.headers.exists(hpred.test)
          } &&
          action.when.body.test(body)

      mocks
        .find(m => routeMatches(m.route))
        .flatMap(m => m.actions.find(actionMatches))
        .map(_.respondWith)
        .getOrElse {
          ResponseDef(
            Status.NotFound,
            request.httpVersion,
            Nil,
            Body.JsonBody(Json.obj("error" := "Not Found", "request" -> Json.obj(
              "method" := request.method,
              "path" := request.uri.path,
              "query" := request.uri.query,
              "headers" := request.headers.headers,
              "body" := body
            )))
          )
        }
    }
}