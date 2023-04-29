package peschke.mock4s.utils

import cats.syntax.all._
import de.marhali.json5._
import io.circe.syntax._
import io.circe.{Encoder, Json}

import scala.jdk.CollectionConverters._

object Json5Codecs {
  private val compatibleOptions = new Json5Options(false, false, false, 0)

  private def fallbackEncoding(element: Json5Element): Json = {
    val rendered = element.toString(compatibleOptions)
    io.circe.parser.parse(rendered).valueOr { e =>
      Json.fromString(
        show"""|Incompatible JSON5
               |======== Rendered JSON5 ==========
               |$rendered
               |======== Parse Errors   ==========
               |$e""".stripMargin
      )
    }
  }

  implicit val json5PrimitiveEncoder: Encoder[Json5Primitive] = Encoder.instance {
    case boolean: Json5Boolean => boolean.getAsBoolean.asJson
    case hexadecimal: Json5Hexadecimal => hexadecimal.getAsBigInteger.asJson
    case number: Json5Number => number.getAsBigDecimal.asJson
    case string: Json5String => string.getAsString.asJson
    case elem => fallbackEncoding(elem)
  }

  implicit val json5ElementEncoder: Encoder[Json5Element] = Encoder.instance {
    case _: Json5Null => Json.Null
    case primitive: Json5Primitive => primitive.asJson
    case array: Json5Array => Json.fromValues(array.iterator().asScala.map(json5ElementEncoder(_)).toVector)
    case json5Object: Json5Object =>
      Json.fromFields {
        json5Object.entrySet().iterator().asScala.map(e => e.getKey -> json5ElementEncoder(e.getValue)).toVector
      }
    case elem => fallbackEncoding(elem)
  }
}
