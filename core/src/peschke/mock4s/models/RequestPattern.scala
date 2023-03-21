package peschke.mock4s.models

import cats.syntax.all._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}
import org.http4s.{Method, Query}
import org.http4s.Uri.Path
import peschke.mock4s.models.RequestPattern.{SimpleMethodPredicate, SimplePathPredicate, SimpleQueryPredicate}
import peschke.mock4s.predicates._
import peschke.mock4s.utils.Circe._

final case class RequestPattern(
                                 method: SimpleMethodPredicate.ADT,
                                 path: SimplePathPredicate.ADT,
                                 query: SimpleQueryPredicate.ADT,
                                 headers: List[HeaderPredicate.Default],
                                 body: BodyPredicate.Default
)
object RequestPattern {
  object SimpleMethodPredicate extends Predicate.Simple[Method]
  object SimplePathPredicate extends Predicate.Simple[Path]
  object SimpleQueryPredicate extends Predicate.Simple[Query]

  implicit val decoder: Decoder[RequestPattern] = accumulatingDecoder { c =>
    (
      c.downField("method").asAcc[SimpleMethodPredicate.ADT],
      c.downField("path").asAcc[SimplePathPredicate.ADT],
      c.downField("query").asAcc[SimpleQueryPredicate.ADT],
      c.downField("headers").asAcc[List[HeaderPredicate.Default]],
      c.downField("body").asAcc[BodyPredicate.Default]
    ).mapN(RequestPattern(
      _,
      _,
      _,
      _,
      _
    ))
  }
  implicit val encoder: Encoder[RequestPattern] = Encoder.instance { pattern =>
    Json.obj(
      "method" := pattern.method,
      "path" := pattern.path,
      "query" := pattern.query,
      "headers" := pattern.headers,
      "body" := pattern.body
    )
  }
}