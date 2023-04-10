package peschke.mock4s.predicates

import cats.syntax.all._
import cats.{Defer, Eq, Order, PartialOrder}
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}
import peschke.mock4s.utils.Circe._
import peschke.mock4s.utils.Orphans._

trait Predicate[A] {
  def test(a: A): Boolean
}
object Predicate   {
  abstract class SimpleEq[A: Eq: Decoder: Encoder] extends PredicateWrapper[A] { self =>
    type Base = Fixed[A] |+| UsingEq[A]

    override implicit val baseDecoder: Decoder[Base] = GeneratedDecoder[Base].decoder
    override implicit val baseEncoder: Encoder[Base] = GeneratedEncoder[Base].encoder

    val always: Type = wrap {
      lhs[Base, UsingCombinators[A, Base]](
        lhs[Fixed[A], UsingEq[A]](Fixed.Always[A]())
      )
    }

    val never: Type = wrap {
      lhs[Base, UsingCombinators[A, Base]](
        lhs[Fixed[A], UsingEq[A]](Fixed.Never[A]())
      )
    }

    def is(sentinel: A): Type = wrap {
      lhs[Base, UsingCombinators[A, Base]](
        rhs[Fixed[A], UsingEq[A]](UsingEq.Is[A](sentinel))
      )
    }

    def in(sentinels: List[A]): Type = wrap {
      lhs[Base, UsingCombinators[A, Base]](
        rhs[Fixed[A], UsingEq[A]](UsingEq.In[A](sentinels))
      )
    }
  }

  abstract class SimpleOrder[A: Order: Decoder: Encoder] extends PredicateWrapper[A] { self =>
    type Base = Fixed[A] |+| UsingEq[A] |+| UsingOrder[A]

    override implicit val baseDecoder: Decoder[Base] = GeneratedDecoder[Base].decoder
    override implicit val baseEncoder: Encoder[Base] = GeneratedEncoder[Base].encoder

    val always: Type = wrap {
      lhs[Base, UsingCombinators[A, Base]](
        lhs[Fixed[A] |+| UsingEq[A], UsingOrder[A]](
          lhs[Fixed[A], UsingEq[A]](Fixed.Always[A]())
        )
      )
    }

    val never: Type = wrap {
      lhs[Base, UsingCombinators[A, Base]](
        lhs[Fixed[A] |+| UsingEq[A], UsingOrder[A]](
          lhs[Fixed[A], UsingEq[A]](Fixed.Never[A]())
        )
      )
    }

    def is(sentinel: A): Type = wrap {
      lhs[Base, UsingCombinators[A, Base]](
        lhs[Fixed[A] |+| UsingEq[A], UsingOrder[A]](
          rhs[Fixed[A], UsingEq[A]](UsingEq.Is[A](sentinel))
        )
      )
    }

    def in(sentinels: List[A]): Type = wrap {
      lhs[Base, UsingCombinators[A, Base]](
        lhs[Fixed[A] |+| UsingEq[A], UsingOrder[A]](
          rhs[Fixed[A], UsingEq[A]](UsingEq.In[A](sentinels))
        )
      )
    }

    def lessThan(sentinel: A): Type = wrap {
      lhs[Base, UsingCombinators[A, Base]](
        rhs[Fixed[A] |+| UsingEq[A], UsingOrder[A]](
          UsingOrder.LessThan[A](sentinel)
        )
      )
    }

    def lessThanEq(sentinel: A): Type = wrap {
      lhs[Base, UsingCombinators[A, Base]](
        rhs[Fixed[A] |+| UsingEq[A], UsingOrder[A]](
          UsingOrder.LessThanEq[A](sentinel)
        )
      )
    }

    def greaterThan(sentinel: A): Type = wrap {
      lhs[Base, UsingCombinators[A, Base]](
        rhs[Fixed[A] |+| UsingEq[A], UsingOrder[A]](
          UsingOrder.GreaterThan[A](sentinel)
        )
      )
    }

    def greaterThanEq(sentinel: A): Type = wrap {
      lhs[Base, UsingCombinators[A, Base]](
        rhs[Fixed[A] |+| UsingEq[A], UsingOrder[A]](
          UsingOrder.GreaterThanEq[A](sentinel)
        )
      )
    }
  }

  sealed trait Fixed[A] extends Predicate[A]

  object Fixed {
    final case class Always[A]() extends Fixed[A] {
      override def test(a: A): Boolean = true
    }
    object Always {
      implicit def decoder[A]: Decoder[Always[A]] =
        fixed("any").as(Always[A]())

      implicit def encoder[A]: Encoder[Always[A]] =
        Encoder.instance(_ => Json.fromString("any"))

      implicit def eq[A]: Eq[Always[A]] = Eq.instance((_, _) => true)
    }

    final case class Never[A]() extends Fixed[A] {
      override def test(a: A): Boolean = false
    }
    object Never {
      implicit def decoder[A]: Decoder[Never[A]] =
        fixed("fail").as(Never[A]())

      implicit def encoder[A]: Encoder[Never[A]] =
        Encoder.instance(_ => Json.fromString("fail"))

      implicit def eq[A]: Eq[Never[A]] = Eq.instance((_, _) => true)
    }

    implicit def decoder[A]: Decoder[Fixed[A]] = anyOf[Fixed[A]](
      Always.decoder[A].widen,
      Never.decoder[A].widen
    )

    implicit def encoder[A]: Encoder[Fixed[A]] = Encoder.instance {
      case p @ Always() => p.asJson
      case p @ Never()  => p.asJson
    }

    implicit def eq[A]: Eq[Fixed[A]] = Eq.instance {
      case (Always(), Always()) => true
      case (Never(), Never())   => true
      case _                    => false
    }
  }

  sealed abstract class UsingEq[A] extends Predicate[A]

  object UsingEq {
    final case class Is[A: Eq](sentinel: A) extends UsingEq[A] {
      override def test(a: A): Boolean = sentinel === a
    }
    object Is {
      implicit def decoder[A: Decoder: Eq]: Decoder[Is[A]] = Decoder[A].map(Is[A]).at("is")

      implicit def encoder[A: Encoder]: Encoder[Is[A]] =
        Encoder.instance(is => Json.obj("is" := is.sentinel))

      implicit def eq[A: Eq]: Eq[Is[A]] = Eq.by(_.sentinel)
    }

    final case class In[A: Eq](sentinels: List[A]) extends UsingEq[A] {
      override def test(a: A): Boolean = sentinels.exists(_ === a)
    }
    object In {
      implicit def decoder[A: Decoder: Eq]: Decoder[In[A]] = Decoder[List[A]].map(In[A]).at("in")

      implicit def encoder[A: Encoder]: Encoder[In[A]] =
        Encoder.instance(in => Json.obj("in" := in.sentinels))

      implicit def eq[A: Eq]: Eq[In[A]] = Eq.by(_.sentinels)
    }

    implicit def decoder[A: Decoder: Eq]: Decoder[UsingEq[A]] = anyOf[UsingEq[A]](
      Is.decoder[A].widen,
      In.decoder[A].widen
    )

    implicit def encoder[A: Encoder]: Encoder[UsingEq[A]] = Encoder.instance {
      case p @ Is(_) => p.asJson
      case p @ In(_) => p.asJson
    }

    implicit def eq[A: Eq]: Eq[UsingEq[A]] = Eq.instance {
      case (a: Is[A], b: Is[A]) => a === b
      case (a: In[A], b: In[A]) => a === b
      case _                    => false
    }
  }

  sealed abstract class UsingOrder[A] extends Predicate[A]
  object UsingOrder {
    final case class LessThan[A: PartialOrder](sentinel: A) extends UsingOrder[A] {
      override def test(a: A): Boolean = a < sentinel
    }
    object LessThan {
      implicit def decoder[A: Decoder: PartialOrder]: Decoder[LessThan[A]] = Decoder[A].map(LessThan[A]).at("<")

      implicit def encoder[A: Encoder]: Encoder[LessThan[A]] =
        Encoder.instance(in => Json.obj("<" := in.sentinel))

      implicit def eq[A: Eq]: Eq[LessThan[A]] = Eq.by(_.sentinel)
    }

    final case class LessThanEq[A: PartialOrder](sentinel: A) extends UsingOrder[A] {
      override def test(a: A): Boolean = a <= sentinel
    }
    object LessThanEq {
      implicit def decoder[A: Decoder: PartialOrder]: Decoder[LessThanEq[A]] = Decoder[A].map(LessThanEq[A]).at("<=")

      implicit def encoder[A: Encoder]: Encoder[LessThanEq[A]] =
        Encoder.instance(in => Json.obj("<=" := in.sentinel))

      implicit def eq[A: Eq]: Eq[LessThanEq[A]] = Eq.by(_.sentinel)
    }

    final case class GreaterThan[A: PartialOrder](sentinel: A) extends UsingOrder[A] {
      override def test(a: A): Boolean = a > sentinel
    }
    object GreaterThan {
      implicit def decoder[A: Decoder: PartialOrder]: Decoder[GreaterThan[A]] = Decoder[A].map(GreaterThan[A]).at(">")

      implicit def encoder[A: Encoder]: Encoder[GreaterThan[A]] =
        Encoder.instance(in => Json.obj(">" := in.sentinel))

      implicit def eq[A: Eq]: Eq[GreaterThan[A]] = Eq.by(_.sentinel)
    }

    final case class GreaterThanEq[A: PartialOrder](sentinel: A) extends UsingOrder[A] {
      override def test(a: A): Boolean = a >= sentinel
    }
    object GreaterThanEq {
      implicit def decoder[A: Decoder: PartialOrder]: Decoder[GreaterThanEq[A]] =
        Decoder[A].map(GreaterThanEq[A]).at(">=")

      implicit def encoder[A: Encoder]: Encoder[GreaterThanEq[A]] =
        Encoder.instance(in => Json.obj(">=" := in.sentinel))

      implicit def eq[A: Eq]: Eq[GreaterThanEq[A]] = Eq.by(_.sentinel)
    }

    implicit def decoder[A: Decoder: PartialOrder]: Decoder[UsingOrder[A]] = anyOf[UsingOrder[A]](
      LessThan.decoder[A].widen,
      LessThanEq.decoder[A].widen,
      GreaterThan.decoder[A].widen,
      GreaterThanEq.decoder[A].widen
    )

    implicit def encoder[A: Encoder]: Encoder[UsingOrder[A]] = Encoder.instance {
      case p @ LessThan(_)      => p.asJson
      case p @ LessThanEq(_)    => p.asJson
      case p @ GreaterThan(_)   => p.asJson
      case p @ GreaterThanEq(_) => p.asJson
    }

    implicit def eq[A: Eq]: Eq[UsingOrder[A]] = Eq.instance {
      case (a: LessThan[A], b: LessThan[A])           => a === b
      case (a: LessThanEq[A], b: LessThanEq[A])       => a === b
      case (a: GreaterThan[A], b: GreaterThan[A])     => a === b
      case (a: GreaterThanEq[A], b: GreaterThanEq[A]) => a === b
      case _                                          => false
    }
  }

  sealed trait UsingCombinators[A, PA <: Predicate[A]] extends Predicate[A]

  object UsingCombinators {
    final case class Wrapped[A, PA <: Predicate[A]](predicate: PA) extends UsingCombinators[A, PA] {
      override def test(a: A): Boolean = predicate.test(a)
    }

    final case class Not[A, PA <: Predicate[A]](combinator: UsingCombinators[A, PA]) extends UsingCombinators[A, PA] {
      override def test(a: A): Boolean = !combinator.test(a)
    }

    final case class ForAll[A, PA <: Predicate[A]](combinators: List[UsingCombinators[A, PA]])
        extends UsingCombinators[A, PA] {
      override def test(a: A): Boolean = combinators.forall(_.test(a))
    }

    final case class Exists[A, PA <: Predicate[A]](combinators: List[UsingCombinators[A, PA]])
        extends UsingCombinators[A, PA] {
      override def test(a: A): Boolean = combinators.exists(_.test(a))
    }

    implicit def eq[A, PA <: Predicate[A]: Eq]: Eq[UsingCombinators[A, PA]] =
      Defer[Eq].fix { implicit eq =>
        Eq.instance {
          case (Wrapped(a), Wrapped(b)) => a === b
          case (Not(a), Not(b))         => a === b
          case (ForAll(a), ForAll(b))   => a === b
          case (Exists(a), Exists(b))   => a === b
          case _                        => false
        }
      }

    implicit def decoder[A, PA <: Predicate[A]](implicit dpa: Decoder[PA]): Decoder[UsingCombinators[A, PA]] =
      Defer[Decoder].fix[UsingCombinators[A, PA]] { implicit duc =>
        val ducL = Decoder[List[UsingCombinators[A, PA]]]
        anyOf(
          duc.map(Not[A, PA]).at("!").widen,
          ducL.map(ForAll[A, PA]).at("forall").widen,
          ducL.map(Exists[A, PA]).at("exists").widen,
          dpa.map(Wrapped[A, PA])
        )
      }

    implicit def encoder[A, PA <: Predicate[A]](implicit epa: Encoder[PA]): Encoder[UsingCombinators[A, PA]] = {
      Defer[Encoder].fix[UsingCombinators[A, PA]] { implicit euc =>
        Encoder.instance[UsingCombinators[A, PA]] {
          case Wrapped(predicate)  => predicate.asJson
          case Not(combinator)     => Json.obj("!" := combinator)
          case ForAll(combinators) => Json.obj("forall" := combinators)
          case Exists(combinators) => Json.obj("exists" := combinators)
        }
      }
    }
  }
}
