package peschke.mock4s.predicates

import cats.Eq
import cats.syntax.all._
import io.circe.Decoder
import io.circe.Encoder
import org.http4s.Header
import org.typelevel.ci.CIString
import peschke.mock4s.algebras.PredicateChecker
import peschke.mock4s.algebras.PredicateChecker.syntax._
import peschke.mock4s.models.|+|
import peschke.mock4s.models.|+|.syntax.LiftOps
import peschke.mock4s.utils.Circe._

final case class HeaderTest(name: CIString, value: StringPredicate.Type)

object HeaderTest {
  implicit val eq: Eq[HeaderTest] = Eq.instance { (a, b) =>
    a.name === b.name && a.value === b.value
  }

  implicit val decoder: Decoder[HeaderTest] =
    Decoder[JsonObjectTuple[CIString, StringPredicate.Type]].map(_.mapN(HeaderTest(_, _)))

  implicit val encoder: Encoder[HeaderTest] = Encoder.instance(ht => JsonObjectTuple.json(ht.name, ht.value))

  implicit val checker: PredicateChecker[Header.Raw, HeaderTest] =
    (predicate, in) => in.name === predicate.name && in.value.satisfies(predicate.value)
}

object HeaderPredicate extends PredicateWrapper[Header.Raw, HeaderTest |+| Fixed[Header.Raw] |+| UsingEq[Header.Raw]] {
  val always: Type = wrap {
    Fixed.Always[Header.Raw]().upcast.second[HeaderTest].first[UsingEq[Header.Raw]].first[UsingCombinators[Base]]
  }

  val never: Type = wrap {
    Fixed.Never[Header.Raw]().upcast.second[HeaderTest].first[UsingEq[Header.Raw]].first[UsingCombinators[Base]]
  }

  def is(sentinel: Header.Raw): Type = wrap {
    UsingEq
      .Is[Header.Raw](sentinel)
      .upcast
      .second[HeaderTest |+| Fixed[Header.Raw]]
      .first[UsingCombinators[Base]]
  }

  def in(sentinels: List[Header.Raw]): Type = wrap {
    UsingEq
      .In[Header.Raw](sentinels)
      .upcast
      .second[HeaderTest |+| Fixed[Header.Raw]]
      .first[UsingCombinators[Base]]
  }

  def header(name: CIString, value: StringPredicate.Type): Type = wrap {
    HeaderTest(name, value)
      .first[Fixed[Header.Raw]]
      .first[UsingEq[Header.Raw]]
      .first[UsingCombinators[Base]]
  }
}
