package peschke.mock4s.predicates

import cats.PartialOrder
import io.circe.Decoder
import io.circe.Encoder
import peschke.mock4s.models.|+|
import peschke.mock4s.models.|+|.syntax.LiftOps

abstract class SimpleOrder[A: Decoder: Encoder: PartialOrder]
    extends PredicateWrapper[A, Fixed[A] |+| UsingEq[A] |+| UsingOrder[A]] {

  val always: Type = wrap {
    Fixed
      .Always[A]().upcast
      .first[UsingEq[A]]
      .first[UsingOrder[A]]
      .first[UsingCombinators[Base]]
  }

  val never: Type = wrap {
    Fixed
      .Never[A]()
      .upcast
      .first[UsingEq[A]]
      .first[UsingOrder[A]]
      .first[UsingCombinators[Base]]
  }

  def is(sentinel: A): Type = wrap {
    UsingEq
      .Is(sentinel)
      .upcast
      .second[Fixed[A]]
      .first[UsingOrder[A]]
      .first[UsingCombinators[Base]]
  }

  def in(sentinels: List[A]): Type = wrap {
    UsingEq
      .In[A](sentinels)
      .upcast
      .second[Fixed[A]]
      .first[UsingOrder[A]]
      .first[UsingCombinators[Base]]
  }

  def lessThan(sentinel: A): Type = wrap {
    UsingOrder.LessThan[A](sentinel).upcast.second[Fixed[A] |+| UsingEq[A]].first[UsingCombinators[Base]]
  }

  def lessThanEq(sentinel: A): Type = wrap {
    UsingOrder.LessThanEq[A](sentinel).upcast.second[Fixed[A] |+| UsingEq[A]].first[UsingCombinators[Base]]
  }

  def greaterThan(sentinel: A): Type = wrap {
    UsingOrder.GreaterThan[A](sentinel).upcast.second[Fixed[A] |+| UsingEq[A]].first[UsingCombinators[Base]]
  }

  def greaterThanEq(sentinel: A): Type = wrap {
    UsingOrder.GreaterThanEq[A](sentinel).upcast.second[Fixed[A] |+| UsingEq[A]].first[UsingCombinators[Base]]
  }
}

object BigDecimalPredicate extends SimpleOrder[BigDecimal]
