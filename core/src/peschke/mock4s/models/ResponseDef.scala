package peschke.mock4s.models

import cats.Eq
import cats.syntax.all._
import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax._
import org.http4s.{Header, HttpVersion, Status}
import peschke.mock4s.utils.Circe._

final case class ResponseDef(status: Status,
                             httpVersionOpt: Option[HttpVersion],
                             headersOpt: Option[List[Header.Raw]],
                             body: Body,
                             updateStateOpt: Option[List[StateTransition]]) {
  val httpVersion: HttpVersion = httpVersionOpt.getOrElse(HttpVersion.`HTTP/1.0`)
  val headers: List[Header.Raw] = headersOpt.getOrElse(Nil)
  val updateState: List[StateTransition] = updateStateOpt.getOrElse(Nil)
}
object ResponseDef {

  val NotFound: ResponseDef = ResponseDef(
    Status.NotFound,
    None,
    None,
    Body.JsonBody(Json.obj("error" := "Not Found", "details" -> Json.obj())),
    None
  )

  implicit val decoder: Decoder[ResponseDef] = accumulatingDecoder { c =>
    def hasKey(key: String): Boolean = c.keys.exists(_.exists(_ === key))
    (
      c.downField("status").asAcc[Status],
      if (hasKey("http-version")) c.downField("http-version").asAcc[HttpVersion].map(_.some)
        else none.valid,
      if (hasKey("headers")) c.downField("headers").asAcc[List[Header.Raw]].map(_.some)
      else none.valid,
      c.downField("body").asAcc[Body],
      if (hasKey("state-updates")) c.downField("state-updates").asAcc[List[StateTransition]].map(_.some)
      else none.valid
    ).mapN(ResponseDef.apply)
  }

  implicit val encoder: Encoder[ResponseDef] = Encoder.instance { responseDef =>
    Json.obj(
      "status"       := responseDef.status,
      "http-version"  := responseDef.httpVersionOpt,
      "headers"      := responseDef.headersOpt,
      "body"         := responseDef.body,
      "state-updates" := responseDef.updateStateOpt
    ).dropNullValues
  }

  implicit val eq: Eq[ResponseDef] = Eq.instance { (a,b) =>
    a.status === b.status &&
      a.httpVersionOpt === b.httpVersionOpt &&
      a.headersOpt === b.headersOpt &&
      a.body === b.body &&
      a.updateStateOpt === b.updateStateOpt
  }
}
