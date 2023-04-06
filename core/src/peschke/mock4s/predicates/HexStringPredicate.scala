package peschke.mock4s.predicates

import io.circe.{Decoder, Encoder}
import peschke.mock4s.models.Body.HexString
import peschke.mock4s.predicates.Predicate.{Fixed, UsingCombinators, UsingEq}
import peschke.mock4s.utils.Circe.{GeneratedDecoder, GeneratedEncoder}

object HexStringPredicate extends PredicateWrapper[HexString] {
  type Base = Fixed[HexString] |+| UsingEq[HexString]

  override implicit val baseDecoder: Decoder[Base] = GeneratedDecoder[Base].decoder
  override implicit val baseEncoder: Encoder[Base] = GeneratedEncoder[Base].encoder

  val always: Type = wrap {
    lhs[Base, UsingCombinators[HexString, Base]](
      lhs[Fixed[HexString], UsingEq[HexString]](Fixed.Always[HexString]())
    )
  }

  val never: Type = wrap {
    lhs[Base, UsingCombinators[HexString, Base]](
      lhs[Fixed[HexString], UsingEq[HexString]](Fixed.Never[HexString]())
    )
  }

  def is(sentinel: HexString): Type = wrap {
    lhs[Base, UsingCombinators[HexString, Base]](
      rhs[Fixed[HexString], UsingEq[HexString]](UsingEq.Is[HexString](sentinel))
    )
  }

  def in(sentinels: List[HexString]): Type = wrap {
    lhs[Base, UsingCombinators[HexString, Base]](
      rhs[Fixed[HexString], UsingEq[HexString]](UsingEq.In[HexString](sentinels))
    )
  }
}
