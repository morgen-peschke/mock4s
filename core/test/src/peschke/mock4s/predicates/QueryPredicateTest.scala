package peschke.mock4s.predicates

import cats.Show
import cats.syntax.all._
import munit.ScalaCheckSuite
import org.http4s.Query
import org.scalacheck.Gen
import org.scalacheck.Prop._

class QueryPredicateTest extends ScalaCheckSuite with MUnitPredicateAsserts {
  implicit val queryShow: Show[Query] = Show.show {
    case Query.Empty => s"Query[Empty](${Query.Empty.renderString})"
    case raw: Query.Raw => s"Query[Raw](${raw.renderString} -> ${raw.pairs})"
    case parsed: Query.Parsed => s"Query[Parsed](${parsed.renderString} -> ${parsed.pairs})"
  }
  def queries(params: Gen[(String, Option[String])]): Gen[Query] = Gen.sized { size =>
    Gen.listOfN(size, params).map(params => Query.fromVector(params.toVector))
  }

  def queries(head: Gen[String], values: Gen[Option[String]]): Gen[Query] = queries(Gen.zip(head, values))

  def queries: Gen[Query] = queries(Gen.alphaStr, Gen.option(Gen.alphaNumStr))

  def queriesWithParam(names: Gen[String], values: Gen[Option[String]]): Gen[(Query, String)] = Gen.sized { size =>
    for {
      query <- Gen.resize(size, queries)
      name <- names
      value <- values
    } yield (query :+ (name -> value)) -> name
  }

  def nonEmptySized[A](max: Int, gen: Gen[A]): Gen[A] =
    Gen.chooseNum(1, max).flatMap(Gen.resize(_, gen))

  def nonEmptyStr(gen: Gen[Char]): Gen[String] = Gen.sized { size =>
    for {
      head <- gen
      tail <- Gen.stringOfN((size - 1).max(0), gen)
    } yield s"$head$tail"
  }

  test("Empty should accept when query was not present") {
    assertAccepts(QueryPredicate.empty, Query.empty)
  }

  test("Empty should reject when query was present, but has no params") {
    assertRejects(QueryPredicate.empty, Query.blank: Query)
  }

  property("Empty should reject queries with parameters") {
    forAll(nonEmptySized(20, queries)) { query =>
      assertRejects(QueryPredicate.empty, query)
    }
  }

  test("OmitsParam should reject when query was not present") {
    assertRejects(QueryPredicate.omitsParam("any-name"), Query.empty)
  }

  property("OmitsParam should accept queries that does not have the parameter") {
    forAll(nonEmptySized(20, queries), nonEmptyStr(Gen.numChar)) { (query, name) =>
      assertAccepts(QueryPredicate.omitsParam(name), query)
    }
  }

  property("OmitsParam should reject queries that do have a specific parameter") {
    forAll(queriesWithParam(nonEmptyStr(Gen.numChar), Gen.option(nonEmptyStr(Gen.numChar)))) { case (query, name) =>
      assertRejects(QueryPredicate.omitsParam(name), query)
    }
  }

  test("EmptyParam should reject when query was not present") {
    assertRejects(QueryPredicate.emptyParam("any-name"), Query.empty)
  }

  property("EmptyParam should accept queries that have the parameter without a value") {
    forAll(queries, nonEmptyStr(Gen.numChar)) { (query, name) =>
      assertAccepts(QueryPredicate.emptyParam(name), query :+ (name -> none))
    }
  }

  property("EmptyParam should reject queries that do not have the parameter") {
    forAll(queries, nonEmptyStr(Gen.numChar)) { (query, name) =>
      assertRejects(QueryPredicate.emptyParam(name), query)
    }
  }

  property("EmptyParam should reject queries that have the parameter, and it has a value") {
    forAll(queriesWithParam(nonEmptyStr(Gen.numChar), nonEmptyStr(Gen.numChar).map(_.some))) { case (query, name) =>
      assertRejects(QueryPredicate.emptyParam(name), query)
    }
  }

  test("ForParam should reject when query was not present") {
    assertRejects(QueryPredicate.forParam("any-name", StringPredicate.always), Query.empty)
  }

  property("ForParam should reject queries that do not have the parameter") {
    forAll(queries, nonEmptyStr(Gen.numChar)) { (query, name) =>
      assertRejects(QueryPredicate.forParam(name, StringPredicate.always), query)
    }
  }

  property("ForParam should reject queries that have the parameter, and it has no value") {
    forAll(queries, nonEmptyStr(Gen.numChar)) { (query, name) =>
      assertRejects(QueryPredicate.forParam(name, StringPredicate.always), query :+ (name -> none))
    }
  }

  property("ForParam should reject queries that have the parameter, and the predicate rejects the value") {
    forAll(queriesWithParam(nonEmptyStr(Gen.numChar), nonEmptyStr(Gen.numChar).map(_.some))) { case (query, name) =>
      assertRejects(QueryPredicate.forParam(name, StringPredicate.never), query)
    }
  }

  property("ForParam should accept queries that have the parameter, and the predicate accepts the value") {
    forAll(queriesWithParam(nonEmptyStr(Gen.numChar), nonEmptyStr(Gen.numChar).map(_.some))) { case (query, name) =>
      assertAccepts(QueryPredicate.forParam(name, StringPredicate.always), query)
    }
  }
}

