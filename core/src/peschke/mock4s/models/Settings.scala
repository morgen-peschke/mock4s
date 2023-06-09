package peschke.mock4s.models

import cats.data.Chain
import cats.data.NonEmptyList
import cats.syntax.all._
import io.circe.CursorOp.DownN
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax._
import peschke.mock4s.models.MockDefinition.MockName
import peschke.mock4s.utils.Circe._

final case class Settings(mocks: Chain[MockDefinition], initialState: MockState.State)
object Settings {
  object MocksWrapper extends supertagged.NewType[Chain[MockDefinition]] {
    implicit val decoder: Decoder[Type] = accumulatingDecoder { c =>
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
                case (_, Chain(_))      => Nil
                case (name, duplicates) =>
                  duplicates.toList.map { case (_, i) =>
                    DecodingFailure(show"Duplicate mock name: $name", DownN(i) :: c.history)
                  }
              }

          NonEmptyList
            .fromList(errors)
            .fold(mocks.map(_._1).validNel[DecodingFailure])(_.invalid)
        }
        .map(apply(_))
    }
  }

  implicit val decoder: Decoder[Settings] = accumulatingDecoder { c =>
    if (c.focus.exists(_.isArray))
      c.asAcc[MocksWrapper.Type].map(MocksWrapper.raw).map(Settings(_, MockState.State.Empty))
    else
      (
        c.downField("mocks").asAcc[MocksWrapper.Type].map(MocksWrapper.raw),
        c.downField("state").asAcc[MockState.State]
      ).mapN(Settings.apply)
  }
  implicit val encoder: Encoder[Settings] = Encoder.instance { settings =>
    Json.obj(
      "mocks" := settings.mocks,
      "state" := settings.initialState
    )
  }
}
