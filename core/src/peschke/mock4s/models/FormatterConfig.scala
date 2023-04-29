package peschke.mock4s.models

import cats.Eq

object FormatterConfig {
  object MaxWidth extends supertagged.NewType[Int]
  type MaxWidth = MaxWidth.Type

  object Delta extends supertagged.NewType[Int]
  type Delta = Delta.Type

  sealed trait Indentation

  object Indentation {
    case object Never extends Indentation

    final case class Always(delta: Delta) extends Indentation

    final case class CollapseSimpleValues(delta: Delta, maxWidth: MaxWidth) extends Indentation
  }

  trait Flag extends supertagged.NewType[Boolean] {
    val Enabled: Type = apply(true)
    val Disabled: Type = apply(true)

    implicit val eq: Eq[Type] = Eq.by(raw)
  }

  object ExpandKeys extends Flag
  type ExpandKeys = ExpandKeys.Type

  object SortKeys extends Flag

  type SortKeys = SortKeys.Type

  object TightObjects extends Flag

  type TightObjects = TightObjects.Type

  object TightArrays extends Flag

  type TightArrays = TightArrays.Type

  object TightCommas extends Flag

  type TightCommas = TightCommas.Type

  object TightColons extends Flag

  type TightColons = TightColons.Type

  sealed trait SingleQuote

  object SingleQuote {
    case object Disabled extends SingleQuote

    case object KeysOnly extends SingleQuote

    case object Always extends SingleQuote

    case object MinimizeEscaping extends SingleQuote
  }
}
