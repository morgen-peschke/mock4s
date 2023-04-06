package peschke.mock4s.predicates

import cats.Eq
import cats.syntax.all._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}
import org.http4s.Header
import org.typelevel.ci.CIString
import peschke.mock4s.predicates.Predicate.{Fixed, UsingCombinators, UsingEq}
import peschke.mock4s.utils.Circe._

object HeaderPredicate extends PredicateWrapper[Header.Raw]{
  case class BaseHeaderPred(name: CIString, value: StringPredicate.Type) extends Predicate[Header.Raw] {
    override def test(a: Header.Raw): Boolean = a.name === name && value.test(a.value)
  }
  implicit val baseEq: Eq[BaseHeaderPred] = Eq.instance { (a, b) =>
    a.name === b.name && a.value === b.value
  }

  implicit def baseDecoder(implicit predDecoder: Decoder[StringPredicate.Type]): Decoder[BaseHeaderPred] =
    accumulatingDecoder { c =>
      (
        c.downField("name").asAcc[CIString],
        c.downField("value").asAcc[StringPredicate.Type]
      ).mapN(BaseHeaderPred.apply)
    }

  implicit def baseEncoder(implicit predEncoder: Encoder[StringPredicate.Type]): Encoder[BaseHeaderPred] =
    Encoder.instance { hp =>
      Json.obj("name" := hp.name, "value" := hp.value)
    }

  type Base = BaseHeaderPred |+| Fixed[Header.Raw] |+| UsingEq[Header.Raw]

  override implicit val baseDecoder: Decoder[Base] = GeneratedDecoder[Base].decoder
  override implicit val baseEncoder: Encoder[Base] = GeneratedEncoder[Base].encoder

  val always: Type = wrap {
    lhs[Base, UsingCombinators[Header.Raw, Base]](
      lhs[BaseHeaderPred |+| Fixed[Header.Raw], UsingEq[Header.Raw]](
        rhs[BaseHeaderPred, Fixed[Header.Raw]](Fixed.Always[Header.Raw]())
      )
    )
  }

  val never: Type = wrap {
    lhs[Base, UsingCombinators[Header.Raw, Base]](
      lhs[BaseHeaderPred |+| Fixed[Header.Raw], UsingEq[Header.Raw]](
        rhs[BaseHeaderPred, Fixed[Header.Raw]](Fixed.Never[Header.Raw]())
      )
    )
  }

  def is(sentinel: Header.Raw): Type = wrap {
    lhs[Base, UsingCombinators[Header.Raw, Base]](
      rhs[BaseHeaderPred |+| Fixed[Header.Raw], UsingEq[Header.Raw]](UsingEq.Is[Header.Raw](sentinel))
    )
  }

  def in(sentinels: List[Header.Raw]): Type = wrap {
    lhs[Base, UsingCombinators[Header.Raw, Base]](
      rhs[BaseHeaderPred |+| Fixed[Header.Raw], UsingEq[Header.Raw]](UsingEq.In[Header.Raw](sentinels))
    )
  }

  def header(name: CIString, value: StringPredicate.Type): Type = wrap {
    lhs[Base, UsingCombinators[Header.Raw, Base]](
      lhs[BaseHeaderPred |+| Fixed[Header.Raw], UsingEq[Header.Raw]](
        lhs[BaseHeaderPred, Fixed[Header.Raw]](BaseHeaderPred(name, value))
      )
    )
  }
}
