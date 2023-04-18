package peschke.mock4s.models

import cats.syntax.all._
import io.circe.Json
import io.circe.syntax._
import munit.FunSuite
import org.http4s.{Method, Status}
import peschke.mock4s.MUnitCirce
import peschke.mock4s.models.MockDefinition.{Action, ActionName}
import peschke.mock4s.predicates.RoutePredicate.MethodPredicate
import peschke.mock4s.predicates.{JsonPredicate, RequestPredicate, RoutePredicate, StatePredicate}

class MockDefinitionJsonTest extends FunSuite with MUnitCirce {
  test("Action codec (simple)") {
    assertCodec(
      Action(
        ActionName("name of action"),
        RequestPredicate.always,
        ResponseDef(
          status = Status.Ok,
          httpVersionOpt = none,
          headersOpt = none,
          body = Body.Empty,
          updateStateOpt = none
        )
      ),
      Json.obj(
        "name" := "name of action",
        "when" := "any",
        "respond-with" := Json.obj(
          "status" := 200,
          "body" := "empty"
        )
      )
    )
  }

  test("Action codec (complex)") {
    assertCodec(
      Action(
        ActionName("name of action"),
        RequestPredicate.forall(List(
          RequestPredicate.route(RoutePredicate.method(MethodPredicate.is(Method.GET))),
          RequestPredicate.state(StatePredicate.isSet(MockState.Key("state-key"), JsonPredicate.is(1.asJson)))
        )),
        ResponseDef(
          status = Status.Ok,
          httpVersionOpt = none,
          headersOpt = none,
          body = Body.Empty,
          updateStateOpt = none
        )
      ),
      Json.obj(
        "name" := "name of action",
        "when" := Json.obj("forall" := List(
          Json.obj("route" := Json.obj("method" := Json.obj("is" := "GET"))),
          Json.obj("state" := Json.obj("key" := "state-key", "value" := Json.obj("is" := 1)))
        )),
        "respond-with" := Json.obj(
          "status" := 200,
          "body" := "empty"
        )
      )
    )
  }
}
