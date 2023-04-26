package peschke.mock4s.models

import cats.Eq
import cats.Order
import cats.Show
import cats.data.Chain
import cats.data.NonEmptyChain
import cats.syntax.all._
import io.circe.ACursor
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.HCursor
import io.circe.Json
import io.circe.syntax._
import peschke.mock4s.models.JsonPath.Segment
import peschke.mock4s.models.JsonPath.Segment.AtIndex
import peschke.mock4s.models.JsonPath.Segment.BareField
import peschke.mock4s.models.JsonPath.Segment.DownArray
import peschke.mock4s.models.JsonPath.Segment.QuotedField
import peschke.mock4s.utils.Circe._
import peschke.mock4s.utils.JsonPathParser

import scala.annotation.tailrec

final case class JsonPath(segments: Chain[JsonPath.Segment]) {
  def <<:(target: Json): Option[Json] = {
    val expectingArray = segments.exists {
      case DownArray => true
      case _         => false
    }
    NonEmptyChain
      .fromChain {
        segments.foldLeft[Chain[Json]](Chain.one(target))(_ <<: _)
      }
      .map { nel =>
        if (!expectingArray && nel.tail.isEmpty) nel.head
        else Json.arr(nel.toNonEmptyVector.toVector: _*)
      }
  }

  def /(index: Int): JsonPath = JsonPath(segments.append(Segment.of(index)))
  def /(field: String): JsonPath = JsonPath(segments.append(Segment.of(field)))
  def /(da: DownArray.type): JsonPath = JsonPath(segments.append(da))

  def render: String =
    segments
      .foldLeft(new StringBuilder() -> none[Segment]) {
        case ((b, None), s)          => b.append('.').append(s.show) -> s.some
        case ((b, Some(last)), curr) =>
          (last, curr) match {
            case (_, BareField(_)) =>
              b.append('.').append(curr.show) -> curr.some
            case _                 => b.append(curr.show) -> curr.some
          }
      }
      ._1
      .result()

  def hCursors(json: Json): Chain[HCursor] = aCursors(json).flatMap(c => Chain.fromOption(c.success))
  def aCursors(json: Json): Chain[ACursor] =
    segments.foldLeft(Chain.one[ACursor](json.hcursor)) { (clist, segment) =>
      segment match {
        case BareField(name)   => clist.map(_.downField(name))
        case QuotedField(name) => clist.map(_.downField(name))
        case AtIndex(value)    => clist.map(_.downN(value))
        case Segment.DownArray =>
          clist.flatMap { c =>
            @tailrec
            def loop(c0: ACursor, accum: Chain[ACursor]): Chain[ACursor] =
              if (c0.failed) accum
              else loop(c0.right, accum.append(c0))

            loop(c.downArray, Chain.empty)
          }
      }
    }
}
object JsonPath                                              {
  sealed trait Segment {
    def <<:(targets: Chain[Json]): Chain[Json] = this match {
      case QuotedField(name)      =>
        targets.flatMap(j => Chain.fromOption(j.hcursor.downField(name).focus))
      case BareField(name)        =>
        targets.flatMap(j => Chain.fromOption(j.hcursor.downField(name).focus))
      case Segment.AtIndex(value) =>
        targets.flatMap(j => Chain.fromOption(j.hcursor.downN(value).focus))
      case DownArray              =>
        targets.flatMap(_.asArray.fold(Chain.empty[Json])(Chain.fromSeq))
    }
  }
  object Segment       {
    final case class BareField(name: String)   extends Segment
    final case class QuotedField(name: String) extends Segment
    final case class AtIndex(value: Int)       extends Segment
    case object DownArray                      extends Segment

    def of(i: Int): Segment = AtIndex(i)
    def of(str: String): Segment =
      if (isValidBareField(str)) BareField(str)
      else QuotedField(str)

    private def isValidBareField(str: String): Boolean =
      str.nonEmpty && str.forall {
        case '\\' | '/' | '"' | '\u0008' | '\u000C' | '\n' | '\t'         => false
        case char if char.isLetterOrDigit || char === '-' || char === '_' => true
        case _                                                            => false
      }

    private def escapeField(field: QuotedField): String =
      field
        .name.toCharArray
        .foldLeft(new StringBuilder()) { (b, c) =>
          c match {
            case '\\'                                                         => b.append(raw"\\")
            case '/'                                                          => b.append("\\/")
            case '"'                                                          => b.append("\\\"")
            case '\u0008'                                                     => b.append("\b")
            case '\u000C'                                                     => b.append("\f")
            case '\n'                                                         => b.append(raw"\n")
            case '\t'                                                         => b.append(raw"\t")
            case char if char.isLetterOrDigit || char === '-' || char === '_' =>
              b.append(char)
            case char                                                         => b.append(f"\\u${char.toInt}%04x")
          }
        }
        .result()

    implicit val catsInstances: Show[Segment] with Eq[Segment] with Order[Segment] =
      new Show[Segment] with Eq[Segment] with Order[Segment] {
        override def eqv(x: Segment, y: Segment): Boolean = (x, y) match {
          case (QuotedField(a), QuotedField(b)) => a === b
          case (BareField(a), BareField(b))     => a === b
          case (AtIndex(a), AtIndex(b))         => a === b
          case (DownArray, DownArray)           => true
          case _                                => false
        }

        override def compare(x: Segment, y: Segment): Int =
          x match {
            case BareField(a)   =>
              y match {
                case BareField(b)                            => a.compare(b)
                case QuotedField(_) | AtIndex(_) | DownArray => -1
              }
            case QuotedField(a) =>
              y match {
                case BareField(_)           => 1
                case QuotedField(b)         => a.compare(b)
                case AtIndex(_) | DownArray => 1
              }
            case AtIndex(a)     =>
              y match {
                case BareField(_) | QuotedField(_) => 1
                case AtIndex(b)                    => a.compare(b)
                case DownArray                     => -1
              }
            case DownArray      =>
              y match {
                case BareField(_) | QuotedField(_) | AtIndex(_) => 1
                case DownArray                                  => 0
              }
          }

        override def show(t: Segment): String = t match {
          case BareField(name)     => name
          case qf @ QuotedField(_) => s"""["${escapeField(qf)}"]"""
          case AtIndex(value)      => s"[$value]"
          case DownArray           => "[]"
        }
      }
  }

  def fromChain(segments: Chain[Segment]): JsonPath = JsonPath(segments)

  def of(segments: Segment*): JsonPath = fromChain(Chain.fromSeq(segments))

  val Root: JsonPath = JsonPath(Chain.empty)

  def paths(json: Json): Chain[JsonPath] = {
    @tailrec
    def walk(queue: Chain[(Json, Chain[Segment])], accum: Chain[JsonPath]): Chain[JsonPath] =
      queue.uncons match {
        case None                                              => accum
        case Some((head @ json -> segmentsSoFar) -> remaining) =>
          val (stop, recurse) =
            json
              .arrayOrObject[Chain[(Json, Chain[Segment])]](
                Chain.one(json -> segmentsSoFar),
                vec =>
                  if (vec.isEmpty) Chain.one(head)
                  else
                    Chain.fromSeq(vec).mapWithIndex { (j, i) =>
                      j -> segmentsSoFar.append(Segment.of(i))
                    },
                obj =>
                  if (obj.isEmpty) Chain.one(head)
                  else
                    Chain
                      .fromSeq(obj.toVector)
                      .map { case (str, j) =>
                        j -> segmentsSoFar.append(Segment.of(str))
                      }
              ).partitionEither { case s @ (json, segments) =>
                if (json.arrayOrObject(false, _.nonEmpty, _.nonEmpty)) s.asRight
                else JsonPath.fromChain(segments).asLeft
              }

          walk(remaining.concat(recurse), accum.concat(stop))
      }

    walk(Chain.one(json -> Chain.empty), Chain.empty)
      .distinct
      .sorted
  }

  def downArrayStylePaths(json: Json): Chain[JsonPath] = {
    paths(json)
      .map(_.segments.map {
        case unchanged @ (BareField(_) | QuotedField(_) | DownArray) => unchanged
        case AtIndex(_)                                              => DownArray
      })
      .map(JsonPath.fromChain)
      .distinct
      .sorted
  }

  implicit val catsInstances: Show[JsonPath] with Eq[JsonPath] with Order[JsonPath] =
    new Show[JsonPath] with Eq[JsonPath] with Order[JsonPath] {
      override def eqv(x: JsonPath, y: JsonPath): Boolean = x.segments === y.segments

      override def compare(x: JsonPath, y: JsonPath): Int = x.segments.compare(y.segments)

      override def show(t: JsonPath): String = t.render
    }

  implicit val decoder: Decoder[JsonPath] = accumulatingDecoder[JsonPath] { c =>
    c.asAcc[String].andThen {
      JsonPathParser.parse(_).leftMap(_.map(pe => DecodingFailure(pe.show, c.history)).toNonEmptyList)
    }
  }

  implicit val encoder: Encoder[JsonPath] = Encoder.instance(_.show.asJson)
}
