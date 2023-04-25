package peschke.mock4s.predicates

import cats.Eq
import io.circe.{Decoder, Encoder}
import peschke.mock4s.models.|+|
import peschke.mock4s.models.|+|.syntax._

abstract class SimpleEq[A: Eq: Decoder: Encoder] extends PredicateWrapper[A, Fixed[A] |+| UsingEq[A]] { self =>
  val always: Type = wrap {
    Fixed
      .Always[A]().upcast
      .first[UsingEq[A]]
      .first[UsingCombinators[Base]]
  }

  val never: Type = wrap {
    Fixed.Never[A]()
      .upcast
      .first[UsingEq[A]]
      .first[UsingCombinators[Base]]
  }

  def is(sentinel: A): Type = wrap {
    UsingEq.Is(sentinel)
      .upcast
      .second[Fixed[A]]
      .first[UsingCombinators[Base]]
  }

  def in(sentinels: List[A]): Type = wrap {
    UsingEq.In[A](sentinels)
      .upcast
      .second[Fixed[A]]
      .first[UsingCombinators[Base]]
  }
}

