package peschke.mock4s.predicates

import cats.Eq
import cats.data.Chain
import cats.syntax.all._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}
import org.http4s.Uri.Path
import org.http4s.Uri.Path.Segment
import peschke.mock4s.predicates.PathPredicate.Token.Literal
import peschke.mock4s.predicates.Predicate.{Fixed, UsingCombinators}
import peschke.mock4s.utils.Circe._
import peschke.mock4s.utils.ChainUtils._

sealed trait PathPredicate extends Predicate[Path]
object PathPredicate       extends PredicateWrapper[Path] {
  sealed trait Token

  object Token {
    final case class Literal(segment: Path.Segment) extends Token

    case object Wildcard extends Token
    val * : Token = Wildcard

    implicit val eq: Eq[Token] = Eq.instance {
      case (Literal(a), Literal(b)) => a === b
      case (Wildcard, Wildcard) => true
      case _ => false
    }

    private [PathPredicate] val semanticEq: Eq[Token] = Eq.instance {
      case (Literal(a), Literal(b)) => a === b
      case _ => true
    }

    private [PathPredicate] val semanticChainEq: Eq[Chain[Token]] = Chain.catsDataEqForChain(semanticEq)
  }

  final case class Sanitized(tokens: Chain[Token], absolute: Boolean, endsWithSlash: Boolean) {
    def equivalentTo(a: Path): Boolean =
      Token.semanticChainEq.eqv(Sanitized.fromLiteralPath(a).tokens, tokens)

    def startsWith(a: Sanitized): Boolean =
      a.absolute === absolute && tokens.startsWith(a.tokens)(Token.semanticEq)

    def starts(a: Path): Boolean = Sanitized.fromLiteralPath(a).startsWith(this)

    def endsWith(a: Sanitized): Boolean =
      a.endsWithSlash === endsWithSlash && tokens.endsWith(a.tokens)(Token.semanticEq)

    def ends(a: Path): Boolean = Sanitized.fromLiteralPath(a).endsWith(this)

    def contains(a: Sanitized): Boolean = tokens.containsEq(a.tokens)(Token.semanticEq)
    def containedBy(a: Path): Boolean = Sanitized.fromLiteralPath(a).contains(this)

    def / (token: Token): Sanitized =  Sanitized(tokens.append(token), absolute, endsWithSlash = false)
    def / (segment: Segment): Sanitized =  this / Literal(segment)
    def / (str: String): Sanitized =
      if (str.isEmpty) copy(endsWithSlash = true)
      else this / Segment(str)
  }

  object Sanitized {
    val Root: Sanitized = Sanitized(Chain.empty, absolute = true, endsWithSlash = false)
    val Empty: Sanitized = Sanitized(Chain.empty, absolute = false, endsWithSlash = false)

    def fromLiteralPath(path: Path): Sanitized =
      Sanitized(
        tokens = Chain.fromSeq(path.segments).map(Token.Literal),
        absolute = path.absolute,
        endsWithSlash = path.endsWithSlash
      )

    implicit val decoder: Decoder[Sanitized] = Decoder[Path].map { path =>
      Sanitized(
        tokens = Chain.fromSeq(path.segments).map { s =>
          if (s.encoded === "*") Token.Wildcard
          else Token.Literal(s)
        },
        absolute = path.absolute,
        endsWithSlash = path.endsWithSlash
      )
    }

    implicit val encoder: Encoder[Sanitized] = Encoder[Path].contramap { sanitized =>
      Path(
        segments = sanitized.tokens.toVector.map {
          case Token.Literal(segment) => segment
          case Token.Wildcard => Path.Segment("*")
        },
        absolute = sanitized.absolute,
        endsWithSlash = sanitized.endsWithSlash
      )
    }

    implicit val eq: Eq[Sanitized] = Eq.instance { (a, b) =>
      a.absolute === b.absolute &&
        a.endsWithSlash === b.endsWithSlash &&
        a.tokens === b.tokens
    }
  }

  final case class Is(sentinel: Either[Sanitized, Path]) extends PathPredicate {
    override def test(a: Path): Boolean = sentinel.fold(_.equivalentTo(a), _ === a)
  }

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

  final case class In(sentinels: Either[List[Sanitized], List[Path]]) extends PathPredicate {
    override def test(a: Path): Boolean = sentinels.fold(_.exists(_.equivalentTo(a)), _.exists(_ === a))
  }

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

  final case class StartsWith(prefix: Either[Sanitized, Path]) extends PathPredicate {
    override def test(a: Path): Boolean = prefix.fold(
      _.starts(a),
      p => p.absolute === a.absolute && a.startsWith(p)
    )
  }

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

  final case class EndsWith(suffix: Either[Sanitized, Path]) extends PathPredicate {
    override def test(a: Path): Boolean = suffix.fold(
      _.ends(a),
      p => a.endsWithSlash === p.endsWithSlash && a.segments.endsWith(p.segments)
    )
  }

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

  final case class Contains(subPath: Either[Sanitized, Path]) extends PathPredicate {
    override def test(a: Path): Boolean = subPath.fold(
      _.containedBy(a),
      p => a.segments.containsSlice(p.segments)
    )
  }

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

  implicit val pathPredicateDecoder: Decoder[PathPredicate] = anyOf(
    Is.decoder.widen,
    In.decoder.widen,
    StartsWith.decoder.widen,
    EndsWith.decoder.widen,
    Contains.decoder.widen
  )

  implicit val pathPredicateEncoder: Encoder[PathPredicate] = Encoder.instance {
    case pp : Is => pp.asJson
    case pp : In => pp.asJson
    case pp : StartsWith => pp.asJson
    case pp : EndsWith => pp.asJson
    case pp : Contains => pp.asJson
  }

  implicit val pathPredicateEq: Eq[PathPredicate] = Eq.instance {
    case (a: Is, b: Is) => a === b
    case (a: In, b: In) => a === b
    case (a: StartsWith, b: StartsWith) => a === b
    case (a: EndsWith, b: EndsWith) => a === b
    case (a: Contains, b: Contains) => a === b
    case _ => false
  }

  override type Base = Fixed[Path] |+| PathPredicate

  override implicit def baseDecoder: Decoder[Base] = GeneratedDecoder[Base].decoder
  override implicit def baseEncoder: Encoder[Base] = GeneratedEncoder[Base].encoder

  val always: Type = wrap {
    lhs[Base, UsingCombinators[Path, Base]](
      lhs[Fixed[Path], PathPredicate](Fixed.Always[Path]())
    )
  }

  val never: Type = wrap {
    lhs[Base, UsingCombinators[Path, Base]](
      lhs[Fixed[Path], PathPredicate](Fixed.Never[Path]())
    )
  }

  def is(sentinel: Path): Type = wrap {
    lhs[Base, UsingCombinators[Path, Base]](
        rhs[Fixed[Path], PathPredicate](Is(sentinel.asRight))
    )
  }

  def isSanitized(sentinel: Sanitized): Type = wrap {
    lhs[Base, UsingCombinators[Path, Base]](
      rhs[Fixed[Path], PathPredicate](Is(sentinel.asLeft))
    )
  }

  def in(sentinels: List[Path]): Type = wrap {
    lhs[Base, UsingCombinators[Path, Base]](
      rhs[Fixed[Path], PathPredicate](In(sentinels.asRight))
    )
  }

  def inSanitized(sentinels: List[Sanitized]): Type = wrap {
    lhs[Base, UsingCombinators[Path, Base]](
      rhs[Fixed[Path], PathPredicate](In(sentinels.asLeft))
    )
  }

  def startsWith(prefix: Path): Type = wrap {
    lhs[Base, UsingCombinators[Path, Base]](
      rhs[Fixed[Path], PathPredicate](StartsWith(prefix.asRight))
    )
  }

  def startsWithSanitized(prefix: Sanitized): Type = wrap {
    lhs[Base, UsingCombinators[Path, Base]](
      rhs[Fixed[Path], PathPredicate](StartsWith(prefix.asLeft))
    )
  }

  def endsWith(suffix: Path): Type = wrap {
    lhs[Base, UsingCombinators[Path, Base]](
      rhs[Fixed[Path], PathPredicate](EndsWith(suffix.asRight))
    )
  }

  def endsWithSanitized(suffix: Sanitized): Type = wrap {
    lhs[Base, UsingCombinators[Path, Base]](
      rhs[Fixed[Path], PathPredicate](EndsWith(suffix.asLeft))
    )
  }

  def contains(subPath: Path): Type = wrap {
    lhs[Base, UsingCombinators[Path, Base]](
      rhs[Fixed[Path], PathPredicate](Contains(subPath.asRight))
    )
  }

  def containsSanitized(subPath: Sanitized): Type = wrap {
    lhs[Base, UsingCombinators[Path, Base]](
      rhs[Fixed[Path], PathPredicate](Contains(subPath.asLeft))
    )
  }
}
