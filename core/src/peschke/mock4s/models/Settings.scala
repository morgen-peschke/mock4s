package peschke.mock4s.models

import cats.data.{Chain, NonEmptyList}
import cats.syntax.all._
import io.circe.CursorOp.DownN
import io.circe.syntax._
import io.circe.{Decoder, DecodingFailure, Encoder}
import peschke.mock4s.models.MockDefinition.MockName
import peschke.mock4s.utils.Circe._

final case class Settings(mocks: Chain[MockDefinition])
object Settings {
  implicit val decoder: Decoder[Settings] = accumulatingDecoder { c =>
    c.asAcc[Chain[MockDefinition]]
      .map {
        _.mapWithIndex { (mock, index) =>
          if (mock.name =!= MockName.Empty) mock -> index
          else mock.copy(name = MockName(s"mock $index")) -> index
        }
      }
      .andThen { mocks =>
        val errors: List[DecodingFailure] =
          mocks
            .groupBy(_._1.name)
            .toList
            .flatMap {
              case (_, Chain(_)) => Nil
              case (name, duplicates) =>
                duplicates.toList.map { case (_, i) =>
                  DecodingFailure(show"Duplicate mock name: $name", DownN(i) :: c.history)
                }
            }

        NonEmptyList.fromList(errors)
          .fold(mocks.map(_._1).validNel[DecodingFailure])(_.invalid)
      }
      .map(Settings.apply)
  }
  implicit val encoder: Encoder[Settings] = Encoder.instance { settings =>
    settings.mocks.asJson
  }
}
