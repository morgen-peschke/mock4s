package peschke.mock4s.models

import cats.Eq
import cats.syntax.all._
import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax._
import org.http4s.Header
import org.http4s.Method
import org.http4s.Query
import org.http4s.Uri
import peschke.mock4s.utils.Circe._

final case class ParsedRequest(route: ParsedRequest.Route, headers: List[Header.Raw], body: ParsedBody)
object ParsedRequest {
  final case class Route(method: Method, path: Uri.Path, query: Query, state: MockState.State)
  object Route {
    implicit val eq: Eq[Route] = Eq.instance { (a, b) =>
      a.method === b.method &&
      a.path === b.path &&
      a.query === b.query
    }

    implicit val decoder: Decoder[Route] = accumulatingDecoder { c =>
      (
        c.downField("method").asAcc[Method],
        c.downField("path").asAcc[Uri.Path],
        c.downField("query").asAcc[Query],
        MockState.State.Empty.validNel
      ).mapN(Route.apply)
    }

    implicit val encoder: Encoder[Route] = Encoder.instance { r =>
      Json.obj(
        "method" := r.method,
        "path"   := r.path,
        "query"  := r.query
      )
    }
  }

  implicit val encoder: Encoder[ParsedRequest] = Encoder.instance { pr =>
    Json.obj(
      "route"   := pr.route,
      "headers" := pr.headers,
      "body"    := pr.body,
      "state"   := pr.route.state
    )
  }
}
