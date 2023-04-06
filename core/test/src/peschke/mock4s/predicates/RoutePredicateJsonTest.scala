package peschke.mock4s.predicates

import io.circe.Json
import io.circe.syntax._
import munit._
import org.http4s.{Method, Query, Uri}
import peschke.mock4s.MUnitCirce
import peschke.mock4s.predicates.RoutePredicate.{MethodPredicate, QueryPredicate}

class RoutePredicateJsonTest extends FunSuite with MUnitCirce {
  test("WhenMethod") {
    assertCodec(
      RoutePredicate.method(MethodPredicate.is(Method.GET)),
      Json.obj("method" := Json.obj("is" := "GET"))
    )
  }

  test("WhenPath") {
    assertCodec(
      RoutePredicate.path(PathPredicate.is(Uri.Path.unsafeFromString("/"))),
      Json.obj("path" := Json.obj("is" := "/"))
    )
  }

  test("WhenQuery") {
    assertCodec(
      RoutePredicate.query(QueryPredicate.is(Query.unsafeFromString("?h=1"))),
      Json.obj("query" := Json.obj("is" := "?h=1"))
    )
  }
}