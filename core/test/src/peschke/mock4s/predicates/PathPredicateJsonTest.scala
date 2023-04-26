package peschke.mock4s.predicates

import io.circe.Json
import io.circe.syntax._
import munit._
import org.http4s.Uri.Path.Root
import peschke.mock4s.MUnitCirce
import peschke.mock4s.models.Sanitized.Token.*
import peschke.mock4s.models.Sanitized.{Empty => SEmpty}
import peschke.mock4s.models.Sanitized.{Root => SRoot}

class PathPredicateJsonTest extends FunSuite with MUnitCirce {
  test("Sanitized relative, no trailing '/'") {
    assertCodec(SEmpty / "foo" / "bar" / *, "foo/bar/*".asJson)
  }

  test("Sanitized relative, with trailing '/'") {
    assertCodec(SEmpty / "foo" / "bar" / * / "", "foo/bar/*/".asJson)
  }

  test("Sanitized absolute, no trailing '/'") {
    assertCodec(SRoot / "foo" / "bar" / *, "/foo/bar/*".asJson)
  }

  test("Sanitized absolute, with trailing '/'") {
    assertCodec(SRoot / "foo" / "bar" / * / "", "/foo/bar/*/".asJson)
  }

  test("is(path)") {
    assertCodec(
      PathPredicate.is(Root / "foo" / "bar" / "baz"),
      Json.obj("is" := "/foo/bar/baz")
    )
  }

  test("is.sanitized(path)") {
    assertCodec(
      PathPredicate.isSanitized(SRoot / "foo" / * / "baz"),
      Json.obj("is.sanitized" := "/foo/*/baz")
    )
  }

  test("in(paths)") {
    assertCodec(
      PathPredicate.in(
        List(
          Root / "foo",
          Root / "foo" / "bar",
          Root / "foo" / "bar" / "baz"
        )
      ),
      Json.obj(
        "in" := Json.arr(
          "/foo".asJson,
          "/foo/bar".asJson,
          "/foo/bar/baz".asJson
        )
      )
    )
  }

  test("in.sanitized(paths)") {
    assertCodec(
      PathPredicate.inSanitized(
        List(
          SRoot / "foo",
          SRoot / "foo" / *,
          SRoot / * / "bar" / "baz"
        )
      ),
      Json.obj(
        "in.sanitized" := Json.arr(
          "/foo".asJson,
          "/foo/*".asJson,
          "/*/bar/baz".asJson
        )
      )
    )
  }

  test("startsWith(path)") {
    assertCodec(
      PathPredicate.startsWith(Root / "foo" / "bar" / "baz"),
      Json.obj("starts-with" := "/foo/bar/baz")
    )
  }

  test("startsWith.sanitized(path)") {
    assertCodec(
      PathPredicate.startsWithSanitized(SRoot / "foo" / * / "baz"),
      Json.obj("starts-with.sanitized" := "/foo/*/baz")
    )
  }

  test("endsWith(path)") {
    assertCodec(
      PathPredicate.endsWith(Root / "foo" / "bar" / "baz"),
      Json.obj("ends-with" := "/foo/bar/baz")
    )
  }

  test("endsWith.sanitized(path)") {
    assertCodec(
      PathPredicate.endsWithSanitized(SRoot / "foo" / * / "baz"),
      Json.obj("ends-with.sanitized" := "/foo/*/baz")
    )
  }

  test("contains(path)") {
    assertCodec(
      PathPredicate.contains(Root / "foo" / "bar" / "baz"),
      Json.obj("contains" := "/foo/bar/baz")
    )
  }

  test("contains.sanitized(path)") {
    assertCodec(
      PathPredicate.containsSanitized(SRoot / "foo" / * / "baz"),
      Json.obj("contains.sanitized" := "/foo/*/baz")
    )
  }
}
