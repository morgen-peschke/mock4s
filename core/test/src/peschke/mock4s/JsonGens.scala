package peschke.mock4s

import cats.syntax.all._
import io.circe.Json
import io.circe.syntax._
import munit.Location
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import peschke.mock4s.models.JsonPath

object JsonGens{
  final case class JsonGenConfig(maxDepth: Int,
                                 minDepth: Int,
                                 maxArrayWidth: Int,
                                 maxObjectWidth: Int,
                                 keys: Gen[String],
                                 scalars: Gen[Json]) {
    def shrink: Gen[JsonGenConfig] = {
      (if (maxDepth > 0) Gen.chooseNum(0, maxDepth - 1) else Gen.const(0))
        .map { smallerMaxDepth =>
          val smallerMinDepth = if (minDepth > 0) minDepth - 1 else 0
          copy(maxDepth = smallerMaxDepth, minDepth = smallerMinDepth)
        }
    }

    def objectWidths: Gen[Int] =
      if (minDepth > 0) Gen.chooseNum(1, maxObjectWidth.max(1))
      else if (maxDepth === 0) Gen.const(0)
      else Gen.chooseNum(0, maxObjectWidth)

    def arrayWidths: Gen[Int] =
      if (minDepth > 0) Gen.chooseNum(1, maxArrayWidth.max(1))
      else if (maxDepth === 0) Gen.const(0)
      else Gen.chooseNum(0, maxArrayWidth)

    def mkGen: Gen[Json] =
      if (minDepth > 0) Gen.oneOf(shrink.flatMap(arrays), shrink.flatMap(objects))
      else Gen.oneOf(scalars, shrink.flatMap(arrays), shrink.flatMap(objects))
  }

  val jsonNulls: Gen[Json] = Gen.const(Json.Null)
  val jsonBooleans: Gen[Json] = arbitrary[Boolean].map(_.asJson)
  val jsonNumbers: Gen[Json] = arbitrary[Long].map(_.asJson)
  val jsonStrings: Gen[Json] = arbitrary[String].map(_.asJson)

  def nonEmptyString(chars: Gen[Char]): Gen[String] =
    Gen.nonEmptyBuildableOf[String, Char](chars)

  val objectKeys: Gen[String] = Gen.resize(10, nonEmptyString(Gen.alphaNumChar))
  val scalars: Gen[Json] = Gen.oneOf(jsonNulls, jsonBooleans, jsonNumbers, Gen.resize(10, jsonStrings))
  val configs: Gen[JsonGenConfig] = Gen.sized { size =>
    for {
      maxDepth <- Gen.chooseNum(0, size)
      minDepth <- if (maxDepth <= 2) Gen.const(2) else Gen.chooseNum(2, maxDepth)
      arrayWidth <- Gen.chooseNum(0, size)
      objectWidth <- Gen.chooseNum(0, size)
    } yield JsonGenConfig(
      maxDepth = maxDepth,
      minDepth = minDepth,
      maxArrayWidth = arrayWidth,
      maxObjectWidth = objectWidth,
      keys = objectKeys,
      scalars = scalars
    )
  }

  val terminals: Gen[Json] = JsonGenConfig(0, 0, 0, 0, objectKeys, scalars).mkGen

  def arrays(config: JsonGenConfig): Gen[Json] =
    config.arrayWidths
      .flatMap(Gen.listOfN(_, config.mkGen))
      .map(_.asJson)

  def objects(config: JsonGenConfig): Gen[Json] = {
    val keyValues = Gen.zip(config.keys, config.mkGen)
    config.objectWidths
      .flatMap(Gen.listOfN(_, keyValues))
      .map(elements => Json.obj(elements: _*))
  }

  def withPath(jsons: Gen[Json])(implicit location: Location): Gen[(Json, JsonPath, Json)] =
    for {
      j <- jsons
      p <- Gen.oneOf(JsonPath.paths(j).toVector)
    } yield {
      val oldValue = MUnitCats.assertSome(j <<: p)
      (j, p, oldValue)
    }

  def withoutPath(jsons: Gen[Json])(implicit location: Location): Gen[(Json, JsonPath, Json)] =
    withPath(jsons).flatMap {
      case (json, path, oldValue) =>
        Gen.oneOf(path.hCursors(json).toVector).map { c =>
          // Can't just delete the value if it's an array, or the index
          // may still be valid, so we replace it with a non-array value
          // so the original path becomes invalid.
          if (c.up.focus.exists(_.isArray))
            (c.up.withFocus(_ => "deleted parent".asJson).root.value, path, oldValue)
          else (c.delete.root.value, path, oldValue)
        }
    }
}
