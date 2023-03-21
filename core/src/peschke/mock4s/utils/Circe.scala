package peschke.mock4s.utils

import cats.data.{Chain, NonEmptyChain, Validated}
import cats.syntax.all._
import cats.{Eq, Semigroup}
import io.circe.Decoder.{AccumulatingResult, Result}
import io.circe.syntax._
import io.circe._
import org.http4s.Uri.Path
import org.http4s.{Header, HttpVersion, Method, Query, Status}
import org.typelevel.ci.CIString

object Circe {
  def accumulatingDecoder[A](f: ACursor => AccumulatingResult[A]): Decoder[A] = new Decoder[A] {
    override def apply(c: HCursor): Result[A] = decodeAccumulating(c).toEither.leftMap(_.head)

    override def decodeAccumulating(c: HCursor): AccumulatingResult[A] = f(c)
  }

  def fixed[A: Eq: Decoder: Encoder](sentinel: A): Decoder[Unit] = {
    val errorMessage = s"Expected ${Encoder[A].apply(sentinel).printWith(Printer.noSpaces)}"
    accumulatingDecoder { c =>
      c.asAcc[A].andThen { value =>
        Validated.condNel(value === sentinel, (), DecodingFailure(errorMessage, c.history))
      }
    }
  }

  implicit def decoderSemigroup[A]: Semigroup[Decoder[A]] =
    Semigroup.instance { (lhs, rhs) =>
      new Decoder[A] {
        final def apply(c: HCursor): Decoder.Result[A] = tryDecode(c)

        override def tryDecode(c: ACursor): Decoder.Result[A] =
          lhs.tryDecode(c).orElse(rhs.tryDecode(c))

        override def decodeAccumulating(c: HCursor): AccumulatingResult[A] =
          tryDecodeAccumulating(c)

        override def tryDecodeAccumulating(c: ACursor): AccumulatingResult[A] =
          lhs.tryDecodeAccumulating(c).recoverWith {
            case lhsFailure => rhs.tryDecodeAccumulating(c).leftMap(lhsFailure.concatNel)
          }
      }
    }

  def anyOf[A](d0: Decoder[A], dN: Decoder[A]*): Decoder[A] =
    NonEmptyChain.fromChainPrepend(d0, Chain.fromSeq(dN)).reduce


  implicit final class DecoderHelpers(private val c: ACursor) extends AnyVal {
    def asAcc[A: Decoder]: AccumulatingResult[A] = Decoder[A].tryDecodeAccumulating(c)
  }

  implicit val ciStringDecoder: Decoder[CIString] = Decoder[String].map(CIString(_))
  implicit val ciStringEncoder: Encoder[CIString] = Encoder[String].contramap(_.toString)

  implicit val methodDecoder: Decoder[Method] = accumulatingDecoder { s =>
    s.asAcc[String].map(_.toUpperCase).andThen { raw =>
      Method.fromString(raw)
        .leftMap(_ => DecodingFailure("Invalid method name", s.history))
        .toValidatedNel
    }
  }
  implicit val methodEncoder: Encoder[Method] = Encoder[String].contramap(_.name)

  implicit val statusDecoder: Decoder[Status] = accumulatingDecoder { c =>
    c.asAcc[Int].andThen(Status.fromInt(_).leftMap(pr => DecodingFailure(pr.message, c.history)).toValidatedNel)
  }
  implicit val statusEncoder: Encoder[Status] = Encoder[Int].contramap(_.code)

  implicit val httpVersionDecoder: Decoder[HttpVersion] = accumulatingDecoder { c =>
    val fromVersionObj = (
      c.downField("major").asAcc[Int],
      c.downField("minor").asAcc[Option[Int]].map(_.getOrElse(0))
    ).mapN(HttpVersion.fromVersion).andThen(_.toValidatedNel)

    fromVersionObj
      .findValid {
        c.asAcc[String].map(HttpVersion.fromString).andThen(_.toValidatedNel)
      }
      .leftMap(_.map { e =>
        DecodingFailure(s"Invalid HTTP Version: ${e.getMessage}", c.history)
      })
  }
  implicit val httpVersionEncoder: Encoder[HttpVersion] = Encoder[String].contramap(_.renderString)

//  implicit val responseClassDecoder: Decoder[Status.ResponseClass] = accumulatingDecoder { c =>
//    c.asAcc[String]
//      .map(_.toLowerCase)
//      .andThen {
//        case "info" => Status.Informational.validNel
//        case "success" => Status.Successful.validNel
//        case "redirect" => Status.Redirection.validNel
//        case "client-error" => Status.ClientError.validNel
//        case "server-error" => Status.ServerError.validNel
//        case _ =>
//          DecodingFailure("Expected one of: info|success|redirect|client-error|server-error", c.history).invalidNel
//      }
//  }
//  implicit val responseClassEncoder: Encoder[Status.ResponseClass] = Encoder.instance {
//    case Status.Informational => "info".asJson
//    case Status.Successful => "success".asJson
//    case Status.Redirection => "redirect".asJson
//    case Status.ClientError => "client-error".asJson
//    case Status.ServerError => "server-error".asJson
//  }

  implicit val headerDecoder: Decoder[Header.Raw] = accumulatingDecoder { h =>
    (h.downField("name").asAcc[String].map(CIString(_)), h.downField("value").asAcc[String]).mapN(Header.Raw.apply)
  }
  implicit val headerEncoder: Encoder[Header.Raw] = Encoder.instance { header =>
    Json.obj(
      "name" := header.name.toString,
      "value" := header.value
    )
  }

  implicit val pathDecoder: Decoder[Path] = accumulatingDecoder(_.asAcc[String].map(Path.unsafeFromString))
  implicit val pathEncoder: Encoder[Path] = Encoder[String].contramap(_.renderString)

  implicit val queryDecoder: Decoder[Query] = accumulatingDecoder { q =>
    q.asAcc[String].andThen { raw =>
      val parsed = Query.unsafeFromString(raw)
      if (parsed === Query.empty && raw.nonEmpty)
        DecodingFailure(s"Invalid query string: $raw", q.history).invalidNel
      else parsed.validNel
    }
  }
  implicit val queryEncoder: Encoder[Query] = Encoder[String].contramap(_.renderString)
}
