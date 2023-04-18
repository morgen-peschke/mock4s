package peschke.mock4s.predicates

import io.circe.Json
import io.circe.syntax._
import munit._
import org.http4s.Method
import org.typelevel.ci._
import peschke.mock4s.MUnitCirce
import peschke.mock4s.models.MockState
import peschke.mock4s.predicates.RequestPredicate.{body, route, state}
import peschke.mock4s.predicates.RoutePredicate.MethodPredicate

class RequestPredicateJsonTest extends FunSuite with MUnitCirce {
  test("WhenRoute") {
    assertCodec(
      route(RoutePredicate.method(MethodPredicate.is(Method.PATCH))),
      Json.obj("route" := Json.obj("method" := Json.obj("is" := "PATCH")))
    )
  }

  test("WhenHeaders") {
    assertCodec(
      RequestPredicate.headers(
        List(
          HeaderPredicate.header(ci"h0", StringPredicate.is("v0")),
          HeaderPredicate.header(ci"h1", StringPredicate.is("v1"))
        )
      ),
      Json.obj(
        "headers" := Json.arr(
          Json.obj(
            "name"  := "h0",
            "value" := Json.obj("is" := "v0")
          ),
          Json.obj(
            "name"  := "h1",
            "value" := Json.obj("is" := "v1")
          )
        )
      )
    )
  }

  test("WhenBody") {
    assertCodec(
      body(BodyPredicate.isEmpty),
      Json.obj("body" := "empty")
    )
  }

  test("WhenState") {
    assertCodec(
      state(StatePredicate.isCleared(MockState.Key("state-key"))),
      Json.obj("state" := Json.obj("cleared" := "state-key"))
    )
  }
}
