package peschke.mock4s.predicates

import cats.Eq
import cats.syntax.all._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}
import org.http4s.Uri.Path
import peschke.mock4s.algebras.PredicateChecker
import peschke.mock4s.models.{Sanitized, |+|}
import peschke.mock4s.models.|+|.syntax.LiftOps
import peschke.mock4s.predicates.PathTest.{Contains, EndsWith, In, Is, StartsWith}
import peschke.mock4s.utils.Circe._

sealed trait PathTest {
  def upcast: PathTest = this
}

object PathTest {
  final case class Is(sentinel: Either[Sanitized, Path]) extends PathTest

  object Is {
    implicit val decoder: Decoder[Is] = anyOf(
      Decoder[Path].at("is").map(s => Is(s.asRight)),
      Decoder[Sanitized].at("is.sanitized").map(s => Is(s.asLeft))
    )

    implicit val encoder: Encoder[Is] = Encoder.instance(
      _.sentinel.fold(
        s => Json.obj("is.sanitized" := s),
        p => Json.obj("is" := p)
      )
    )

    implicit val eq: Eq[Is] = Eq.by(_.sentinel)
  }

  final case class In(sentinels: Either[List[Sanitized], List[Path]]) extends PathTest

  object In {
    implicit val decoder: Decoder[In] = anyOf(
      Decoder[List[Path]].at("in").map(lp => In(lp.asRight)),
      Decoder[List[Sanitized]].at("in.sanitized").map(ls => In(ls.asLeft))
    )

    implicit val encoder: Encoder[In] = Encoder.instance(
      _.sentinels.fold(
        ls => Json.obj("in.sanitized" := ls),
        lp => Json.obj("in" := lp)
      )
    )

    implicit val eq: Eq[In] = Eq.by(_.sentinels)
  }

  final case class StartsWith(prefix: Either[Sanitized, Path]) extends PathTest

  object StartsWith {
    implicit val decoder: Decoder[StartsWith] = anyOf(
      Decoder[Path].at("starts-with").map(p => StartsWith(p.asRight)),
      Decoder[Sanitized].at("starts-with.sanitized").map(s => StartsWith(s.asLeft))
    )

    implicit val encoder: Encoder[StartsWith] = Encoder.instance(
      _.prefix.fold(
        s => Json.obj("starts-with.sanitized" := s),
        p => Json.obj("starts-with" := p)
      )
    )

    implicit val eq: Eq[StartsWith] = Eq.by(_.prefix)
  }

  final case class EndsWith(suffix: Either[Sanitized, Path]) extends PathTest

  object EndsWith {
    implicit val decoder: Decoder[EndsWith] = anyOf(
      Decoder[Path].at("ends-with").map(p => EndsWith(p.asRight)),
      Decoder[Sanitized].at("ends-with.sanitized").map(s => EndsWith(s.asLeft))
    )

    implicit val encoder: Encoder[EndsWith] = Encoder.instance(
      _.suffix.fold(
        s => Json.obj("ends-with.sanitized" := s),
        p => Json.obj("ends-with" := p)
      )
    )

    implicit val eq: Eq[EndsWith] = Eq.by(_.suffix)
  }

  final case class Contains(subPath: Either[Sanitized, Path]) extends PathTest

  object Contains {
    implicit val decoder: Decoder[Contains] = anyOf(
      Decoder[Path].at("contains").map(p => Contains(p.asRight)),
      Decoder[Sanitized].at("contains.sanitized").map(s => Contains(s.asLeft))
    )

    implicit val encoder: Encoder[Contains] = Encoder.instance(
      _.subPath.fold(
        s => Json.obj("contains.sanitized" := s),
        p => Json.obj("contains" := p)
      )
    )

    implicit val eq: Eq[Contains] = Eq.by(_.subPath)
  }

  implicit val decoder: Decoder[PathTest] = anyOf(
    Is.decoder.widen,
    In.decoder.widen,
    StartsWith.decoder.widen,
    EndsWith.decoder.widen,
    Contains.decoder.widen
  )

  implicit val encoder: Encoder[PathTest] = Encoder.instance {
    case pp: Is => pp.asJson
    case pp: In => pp.asJson
    case pp: StartsWith => pp.asJson
    case pp: EndsWith => pp.asJson
    case pp: Contains => pp.asJson
  }

  implicit val eq: Eq[PathTest] = Eq.instance {
    case (a: Is, b: Is) => a === b
    case (a: In, b: In) => a === b
    case (a: StartsWith, b: StartsWith) => a === b
    case (a: EndsWith, b: EndsWith) => a === b
    case (a: Contains, b: Contains) => a === b
    case _ => false
  }

  implicit val checker: PredicateChecker[Path, PathTest] = (predicate, in) => predicate match {
    case Is(sentinel) => sentinel.fold(_.equivalentTo(in), _ === in)
    case In(sentinels) => sentinels.fold(_.exists(_.equivalentTo(in)), _.exists(_ === in))
    case StartsWith(prefix) => prefix.fold(_.starts(in), p => p.absolute === in.absolute && in.startsWith(p))
    case EndsWith(suffix) => suffix.fold(
      _.ends(in),
      p => in.endsWithSlash === p.endsWithSlash && in.segments.endsWith(p.segments)
    )
    case Contains(subPath) => subPath.fold(_.containedBy(in), p => in.segments.containsSlice(p.segments))
  }
}

object PathPredicate extends PredicateWrapper[Path, Fixed[Path] |+| PathTest] {
  val always: Type = wrap(Fixed.Always[Path]().upcast.first[PathTest].first[UsingCombinators[Base]])

  val never: Type = wrap(Fixed.Never[Path]().upcast.first[PathTest].first[UsingCombinators[Base]])

  def is(sentinel: Path): Type = wrap(Is(sentinel.asRight).upcast.second[Fixed[Path]].first[UsingCombinators[Base]])

  def isSanitized(sentinel: Sanitized): Type =
    wrap(Is(sentinel.asLeft).upcast.second[Fixed[Path]].first[UsingCombinators[Base]])

  def in(sentinels: List[Path]): Type =
    wrap(In(sentinels.asRight).upcast.second[Fixed[Path]].first[UsingCombinators[Base]])

  def inSanitized(sentinels: List[Sanitized]): Type =
    wrap(In(sentinels.asLeft).upcast.second[Fixed[Path]].first[UsingCombinators[Base]])

  def startsWith(prefix: Path): Type =
    wrap(StartsWith(prefix.asRight).upcast.second[Fixed[Path]].first[UsingCombinators[Base]])

  def startsWithSanitized(prefix: Sanitized): Type =
    wrap(StartsWith(prefix.asLeft).upcast.second[Fixed[Path]].first[UsingCombinators[Base]])

  def endsWith(suffix: Path): Type =
    wrap(EndsWith(suffix.asRight).upcast.second[Fixed[Path]].first[UsingCombinators[Base]])

  def endsWithSanitized(suffix: Sanitized): Type =
    wrap(EndsWith(suffix.asLeft).upcast.second[Fixed[Path]].first[UsingCombinators[Base]])

  def contains(subPath: Path): Type =
    wrap(Contains(subPath.asRight).upcast.second[Fixed[Path]].first[UsingCombinators[Base]])

  def containsSanitized(subPath: Sanitized): Type =
    wrap(Contains(subPath.asLeft).upcast.second[Fixed[Path]].first[UsingCombinators[Base]])
}
