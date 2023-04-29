package peschke.mock4s.algebras

import cats.Applicative
import cats.syntax.all._
import io.circe.{Json, JsonNumber, JsonObject}
import peschke.mock4s.models.JsonPath.Segment
import peschke.mock4s.utils.JsonPathParser

trait JsonKeyExpander[F[_]]{
  def expand(json: Json): F[Json]
}
object JsonKeyExpander {

  def noOp[F[_]: Applicative]: JsonKeyExpander[F] = _.pure[F]

  def default[F[_]: Applicative]: JsonKeyExpander[F] = new JsonKeyExpander[F] {
    private val folder: Json.Folder[Json] = new Json.Folder[Json] {
      override def onNull: Json = Json.Null

      override def onBoolean(value: Boolean): Json = Json.fromBoolean(value)

      override def onNumber(value: JsonNumber): Json = Json.fromJsonNumber(value)

      override def onString(value: String): Json = Json.fromString(value)

      override def onArray(value: Vector[Json]): Json = Json.fromValues(value.map(_.foldWith(this)))

      override def onObject(value: JsonObject): Json = Json.fromFields(value.toIterable.map {
        case (key, value) =>
          val foldedValue = value.foldWith(this)
          def notExpandable = key -> foldedValue

          if (!key.startsWith("$")) notExpandable
          else JsonPathParser.parse(key).toOption.flatMap { path =>
            val fieldNames = path.segments.map {
              case Segment.BareField(name) => name.some.some
              case Segment.QuotedField(name) => name.some.some
              case Segment.DownArray => none.some
              case Segment.AtIndex(_) => none
            }
            fieldNames.uncons match {
              case Some(Some(Some(head)) -> tail) =>
                tail
                  .reverse
                  .foldLeftM(foldedValue) { (accum, pathPartOpt) =>
                    pathPartOpt.map {
                      case Some(name) => Json.obj(name -> accum)
                      case None => Json.arr(accum)
                    }
                  }
                  .map(head -> _)
              case _ => None
            }
          }.getOrElse(notExpandable)
      })
    }

    override def expand(json: Json): F[Json] = json.foldWith(folder).pure[F]
  }
}
