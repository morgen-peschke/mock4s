package peschke.mock4s.models

import cats.Eq
import cats.data.Chain
import cats.syntax.all._
import io.circe.{Decoder, Encoder}
import org.http4s.Uri.Path
import org.http4s.Uri.Path.Segment
import peschke.mock4s.models.Sanitized.Token
import peschke.mock4s.models.Sanitized.Token.Literal
import peschke.mock4s.utils.ChainUtils._
import peschke.mock4s.utils.Circe._

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

  def /(token: Token): Sanitized = Sanitized(tokens.append(token), absolute, endsWithSlash = false)

  def /(segment: Segment): Sanitized = this / Literal(segment)

  def /(str: String): Sanitized =
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

    val semanticEq: Eq[Token] = Eq.instance {
      case (Literal(a), Literal(b)) => a === b
      case _ => true
    }

    val semanticChainEq: Eq[Chain[Token]] = Chain.catsDataEqForChain(semanticEq)
  }

  implicit val decoder: Decoder[Sanitized] = Decoder[Path].map { path =>
    Sanitized(
      tokens = Chain.fromSeq(path.segments).map { s =>
        if (s.encoded === "*") Token.Wildcard
        else Literal(s)
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