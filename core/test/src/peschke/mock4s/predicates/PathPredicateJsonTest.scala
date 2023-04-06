package peschke.mock4s.predicates

import io.circe.Json
import io.circe.syntax._
import munit._
import peschke.mock4s.MUnitCirce
import peschke.mock4s.predicates.PathPredicate.Sanitized
import peschke.mock4s.predicates.PathPredicate.Token.{Literal, Wildcard}
import org.http4s.Uri.Path.Segment

class PathPredicateJsonTest extends FunSuite with MUnitCirce {
  test("Sanitized relative, no trailing '/'") {
    assertCodec(
      PathPredicate.sanitized(Sanitized(
        tokens = Literal(Segment("foo")) :: Literal(Segment("bar")) :: Wildcard :: Nil,
        absolute = false,
        endsWithSlash = false
      )),
      Json.obj("sanitized" := "foo/bar/*" )
    )
  }

  test("Sanitized relative, with trailing '/'") {
    assertCodec(
      PathPredicate.sanitized(Sanitized(
        tokens = Literal(Segment("foo")) :: Literal(Segment("bar")) :: Wildcard :: Nil,
        absolute = false,
        endsWithSlash = true
      )),
      Json.obj("sanitized" := "foo/bar/*/")
    )
  }

  test("Sanitized absolute, no trailing '/'") {
    assertCodec(
      PathPredicate.sanitized(Sanitized(
        tokens = Literal(Segment("foo")) :: Literal(Segment("bar")) :: Wildcard :: Nil,
        absolute = true,
        endsWithSlash = false
      )),
      Json.obj("sanitized" := "/foo/bar/*")
    )
  }

  test("Sanitized absolute, with trailing '/'") {
    assertCodec(
      PathPredicate.sanitized(Sanitized(
        tokens = Literal(Segment("foo")) :: Literal(Segment("bar")) :: Wildcard :: Nil,
        absolute = true,
        endsWithSlash = true
      )),
      Json.obj("sanitized" := "/foo/bar/*/")
    )
  }
}
