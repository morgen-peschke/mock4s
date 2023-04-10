package peschke.mock4s.models

import cats.syntax.all._
import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax._
import org.http4s.{Header, HttpVersion, Status}
import peschke.mock4s.utils.Circe._

final case class ResponseDef(status: Status, httpVersion: HttpVersion, headers: List[Header.Raw], body: Body)
object ResponseDef {

  val NotFound: ResponseDef = ResponseDef(
    Status.NotFound,
    HttpVersion.`HTTP/1.0`,
    Nil,
    Body.JsonBody(Json.obj("error" := "Not Found", "details" -> Json.obj()))
  )

  implicit val decoder: Decoder[ResponseDef] = accumulatingDecoder { c =>
    (
      c.downField("status").asAcc[Status],
      c.downField("httpVersion").asAcc[Option[HttpVersion]].map(_.getOrElse(HttpVersion.`HTTP/1.0`)),
      c.downField("headers").asAcc[List[Header.Raw]],
      c.downField("body").asAcc[Body]
    ).mapN(ResponseDef.apply)
  }
  implicit val encoder: Encoder[ResponseDef] = Encoder.instance { responseDef =>
    Json.obj(
      "status"      := responseDef.status,
      "httpVersion" := responseDef.httpVersion,
      "headers"     := responseDef.headers,
      "body"        := responseDef.body
    )
  }
}
