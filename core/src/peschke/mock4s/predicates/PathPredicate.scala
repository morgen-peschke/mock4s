package peschke.mock4s.predicates

import cats.Eq
import cats.syntax.all._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}
import org.http4s.Uri.Path
import peschke.mock4s.predicates.Predicate.{Fixed, UsingCombinators, UsingEq}
import peschke.mock4s.utils.Circe._

import scala.annotation.tailrec

sealed trait PathPredicate extends Predicate[Path]
object PathPredicate       extends PredicateWrapper[Path] {
  sealed trait Token
  object Token {
    final case class Literal(segment: Path.Segment) extends Token
    case object Wildcard                            extends Token

    implicit val eq: Eq[Token] = Eq.instance {
      case (Literal(a), Literal(b)) => a === b
      case (Wildcard, Wildcard)     => true
      case _                        => false
    }
  }

  final case class Sanitized(tokens: List[Token], absolute: Boolean, endsWithSlash: Boolean) extends PathPredicate {
    override def test(a: Path): Boolean = {
      @tailrec
      def loop(path: List[Path.Segment], remaining: List[Token]): Boolean =
        path match {
          case Nil                   => remaining.isEmpty
          case segment :: restOfPath =>
            remaining match {
              case (Token.Literal(`segment`) | Token.Wildcard) :: restOfTokens =>
                loop(restOfPath, restOfTokens)
              case _                                                           => false
            }
        }

      a.absolute === absolute && a.endsWithSlash && endsWithSlash && loop(a.segments.toList, tokens)
    }
  }
  object Sanitized {
    implicit val decoder: Decoder[Sanitized] = Decoder[Path].map { path =>
      Sanitized(
        tokens = path.segments.toList.map { s =>
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
          case Token.Wildcard         => Path.Segment("*")
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

  implicit val pathPredicateDecoder: Decoder[PathPredicate] =
    Decoder[Sanitized].at("sanitized").widen

  implicit val pathPredicateEncoder: Encoder[PathPredicate] = Encoder.instance { case s @ Sanitized(_, _, _) =>
    Json.obj("sanitized" := s)
  }

  implicit val pathPredicateEq: Eq[PathPredicate] = Eq.instance {
    case (a @ Sanitized(_, _, _), b @ Sanitized(_, _, _)) => a === b
  }

  override type Base = Fixed[Path] |+| UsingEq[Path] |+| PathPredicate

  override implicit def baseDecoder: Decoder[Base] = GeneratedDecoder[Base].decoder
  override implicit def baseEncoder: Encoder[Base] = GeneratedEncoder[Base].encoder

  val always: Type = wrap {
    lhs[Base, UsingCombinators[Path, Base]](
      lhs[Fixed[Path] |+| UsingEq[Path], PathPredicate](
        lhs[Fixed[Path], UsingEq[Path]](Fixed.Always[Path]())
      )
    )
  }

  val never: Type = wrap {
    lhs[Base, UsingCombinators[Path, Base]](
      lhs[Fixed[Path] |+| UsingEq[Path], PathPredicate](
        lhs[Fixed[Path], UsingEq[Path]](Fixed.Never[Path]())
      )
    )
  }

  def is(sentinel: Path): Type = wrap {
    lhs[Base, UsingCombinators[Path, Base]](
      lhs[Fixed[Path] |+| UsingEq[Path], PathPredicate](
        rhs[Fixed[Path], UsingEq[Path]](UsingEq.Is[Path](sentinel))
      )
    )
  }

  def in(sentinels: List[Path]): Type = wrap {
    lhs[Base, UsingCombinators[Path, Base]](
      lhs[Fixed[Path] |+| UsingEq[Path], PathPredicate](
        rhs[Fixed[Path], UsingEq[Path]](UsingEq.In[Path](sentinels))
      )
    )
  }

  def sanitized(p: Sanitized): Type = wrap {
    lhs[Base, UsingCombinators[Path, Base]](
      rhs[Fixed[Path] |+| UsingEq[Path], PathPredicate](p)
    )
  }
}
