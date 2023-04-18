package peschke.mock4s.models

import cats.syntax.all._
import io.circe.Json
import io.circe.syntax._
import munit.FunSuite
import org.http4s.{Header, HttpVersion, Status}
import org.typelevel.ci._
import peschke.mock4s.MUnitCirce

class ResponseDefJsonTest extends FunSuite with MUnitCirce {
  test("completely filled in") {
    assertCodec(
      ResponseDef(
        status = Status.Ok,
        httpVersionOpt = HttpVersion.`HTTP/1.1`.some,
        headersOpt = List(
          Header.Raw(ci"Content-Type", "application/json")
        ).some,
        body = Body.JsonBody(Json.obj("in-body" := Json.Null)),
        updateStateOpt = List(
          StateTransition.Set(MockState.Key("state-key"), Json.Null)
        ).some
      ),
      Json.obj(
        "status" := 200,
        "http-version" := "HTTP/1.1",
        "headers" := Json.arr(
          Json.obj("name" := "Content-Type", "value" := "application/json")
        ),
        "body" := Json.obj("json" := Json.obj("in-body" := Json.Null)),
        "state-updates" := Json.arr(
          Json.obj("set" := Json.obj("key":= "state-key", "value" := Json.Null))
        )
      )
    )
  }

  test("allow missing HttpVersion") {
    assertCodec(
      ResponseDef(
        status = Status.Ok,
        httpVersionOpt = none,
        headersOpt = List(
          Header.Raw(ci"Content-Type", "application/json")
        ).some,
        body = Body.JsonBody(Json.obj("in-body" := Json.Null)),
        updateStateOpt = List(
          StateTransition.Set(MockState.Key("state-key"), Json.Null)
        ).some
      ),
      Json.obj(
        "status" := 200,
        "headers" := Json.arr(
          Json.obj("name" := "Content-Type", "value" := "application/json")
        ),
        "body" := Json.obj("json" := Json.obj("in-body" := Json.Null)),
        "state-updates" := Json.arr(
          Json.obj("set" := Json.obj("key":= "state-key", "value" := Json.Null))
        )
      )
    )
  }

  test("allows missing headers") {
    assertCodec(
      ResponseDef(
        status = Status.Ok,
        httpVersionOpt = none,
        headersOpt = none,
        body = Body.JsonBody(Json.obj("in-body" := Json.Null)),
        updateStateOpt = List(
          StateTransition.Set(MockState.Key("state-key"), Json.Null)
        ).some
      ),
      Json.obj(
        "status" := 200,
        "body" := Json.obj("json" := Json.obj("in-body" := Json.Null)),
        "state-updates" := Json.arr(
          Json.obj("set" := Json.obj("key":= "state-key", "value" := Json.Null))
        )
      )
    )
  }

  test("allows missing state updates") {
    assertCodec(
      ResponseDef(
        status = Status.Ok,
        httpVersionOpt = none,
        headersOpt = none,
        body = Body.JsonBody(Json.obj("in-body" := Json.Null)),
        updateStateOpt = none
      ),
      Json.obj(
        "status" := 200,
        "body" := Json.obj("json" := Json.obj("in-body" := Json.Null))
      )
    )
  }
}
