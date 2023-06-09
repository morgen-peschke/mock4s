package peschke.mock4s.predicates

import io.circe.Json
import io.circe.syntax._
import munit._
import org.http4s.Method
import org.http4s.Query
import org.http4s.Uri
import peschke.mock4s.MUnitCirce
import peschke.mock4s.models.MockState
import peschke.mock4s.predicates.RoutePredicate.method
import peschke.mock4s.predicates.RoutePredicate.path
import peschke.mock4s.predicates.RoutePredicate.query
import peschke.mock4s.predicates.RoutePredicate.state

class RoutePredicateJsonTest extends FunSuite with MUnitCirce {
  test("WhenMethod") {
    assertCodec(
      method(MethodPredicate.is(Method.GET)),
      Json.obj("method" := Json.obj("is" := "GET"))
    )
  }

  test("WhenPath") {
    assertCodec(
      path(PathPredicate.is(Uri.Path.unsafeFromString("/"))),
      Json.obj("path" := Json.obj("is" := "/"))
    )
  }

  test("WhenQuery") {
    assertCodec(
      query(QueryPredicate.is(Query.unsafeFromString("?h=1"))),
      Json.obj("query" := Json.obj("is" := "?h=1"))
    )
  }

  test("WhenState") {
    assertCodec(
      state(StatePredicate.isCleared(MockState.Key("state-key"))),
      Json.obj("state" := Json.obj("cleared" := "state-key"))
    )
  }
}
