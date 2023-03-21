package peschke.mock4s.predicates

import cats.syntax.all._
import cats.{Eq, Order}
import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import io.circe.syntax._
import peschke.mock4s.predicates.PredicateADT.{Always, Exists, ForAll, GreaterThan, GreaterThanEq, In, Is, LessThan, LessThanEq, Never, Not, When}
import peschke.mock4s.utils.Circe._

import scala.reflect.ClassTag

trait Predicate[A] {
  def test(a: A): Boolean
}
object Predicate {
  def of[A](p: A => Boolean): Predicate[A] = p(_)

  abstract class Simple[A](implicit ct: ClassTag[A]) extends Predicate[A] { self =>
    override def test(a: A): Boolean = false
    implicit val decoder: Decoder[self.type] =
      accumulatingDecoder(c => DecodingFailure(s"No predicates for $ct", c.history).invalidNel)

    implicit val encoder: Encoder[self.type] = Encoder.instance(_ => Json.Null)

    type ADT = PredicateADT.Aux[A, self.type]
  }
}

sealed abstract class PredicateADT[A](val test: A => Boolean) {
  type PredA <: Predicate[A]
}

trait PredicateADTDecoderWithoutEq {
  implicit def decoderUsingOnlyPredicate[A, PA <: Predicate[A]](implicit DA: Decoder[A],
                                 DPA: Decoder[PA]): Decoder[PredicateADT.Aux[A, PA]] =
    anyOf[PredicateADT.Aux[A, PA]](
      Always.decoder[A, PA].widen,
      Never.decoder[A, PA].widen,
      Not.decoder[A, PA](decoderUsingOnlyPredicate[A, PA]).widen,
      ForAll.decoder[A, PA](decoderUsingOnlyPredicate[A, PA]).widen,
      Exists.decoder[A, PA](decoderUsingOnlyPredicate[A, PA]).widen,
      When.decoder[A, PA].widen
    )
}

trait PredicateADTDecoderWithEq extends PredicateADTDecoderWithoutEq {
  implicit def decoderUsingEq[A, PA <: Predicate[A]](implicit DA: Decoder[A],
                                 E: Eq[A],
                                 DPA: Decoder[PA]): Decoder[PredicateADT.Aux[A, PA]] =
    anyOf[PredicateADT.Aux[A, PA]](
      Always.decoder[A, PA].widen,
      Never.decoder[A, PA].widen,
      Is.decoder[A, PA].widen,
      In.decoder[A, PA].widen,
      Not.decoder[A, PA](decoderUsingEq[A, PA]).widen,
      ForAll.decoder[A, PA](decoderUsingEq[A, PA]).widen,
      Exists.decoder[A, PA](decoderUsingEq[A, PA]).widen,
      When.decoder[A, PA].widen
    )
}

trait PredicateADTDecoderWithOrder extends PredicateADTDecoderWithEq {
  implicit def decoderUsingOrder[A, PA <: Predicate[A]](implicit
                                                        DA: Decoder[A],
                                                        O: Order[A],
                                                        DPA: Decoder[PA]): Decoder[PredicateADT.Aux[A, PA]] =
    anyOf[PredicateADT.Aux[A, PA]](
      Always.decoder[A, PA].widen,
      Never.decoder[A, PA].widen,
      Is.decoder[A, PA].widen,
      In.decoder[A, PA].widen,
      LessThan.decoder[A, PA].widen,
      LessThanEq.decoder[A, PA].widen,
      GreaterThan.decoder[A, PA].widen,
      GreaterThanEq.decoder[A, PA].widen,
      Not.decoder[A, PA](decoderUsingOrder[A, PA]).widen,
      ForAll.decoder[A, PA](decoderUsingOrder[A, PA]).widen,
      Exists.decoder[A, PA](decoderUsingOrder[A, PA]).widen,
      When.decoder[A, PA].widen
    )
}
trait PredicateADTEncoder extends PredicateADTDecoderWithOrder {
  implicit def encoder[A, PA <: Predicate[A]](implicit
                                              EA: Encoder[A],
                                              EPA: Encoder[PA]): Encoder[PredicateADT.Aux[A, PA]] =
    Encoder.instance[PredicateADT.Aux[A, PA]] {
      case padt: Always[A, PA] => padt.asJson
      case padt: Never[A, PA] =>  padt.asJson
      case padt: Is[A, PA] => padt.asJson
      case padt: In[A, PA] => padt.asJson
      case padt: LessThan[A, PA] =>  padt.asJson
      case padt: LessThanEq[A, PA] =>  padt.asJson
      case padt: GreaterThan[A, PA] =>  padt.asJson
      case padt: GreaterThanEq[A, PA] =>  padt.asJson
      case padt: Not[A, PA] => Not.encoder(encoder(EA, EPA))(padt)
      case padt: ForAll[A, PA] => ForAll.encoder(encoder(EA, EPA))(padt)
      case padt: Exists[A, PA] => Exists.encoder(encoder(EA, EPA))(padt)
      case padt: When[A, PA] => padt.asJson
    }
}
object PredicateADT extends PredicateADTEncoder {
  type Aux[A, PA <: Predicate[A]] = PredicateADT[A] {
    type PredA = PA
  }

  final case class When[A, PA <: Predicate[A]](predicate: PA) extends PredicateADT[A](predicate.test) {
    override type PredA = PA
  }
  object When {
    implicit def decoder[A, PA <: Predicate[A]](implicit predDecoder: Decoder[PA]): Decoder[When[A, PA]] =
      accumulatingDecoder(_.asAcc[PA].map(When[A, PA])).at("when")

    implicit def encoder[A, PA <: Predicate[A]](implicit predEncoder: Encoder[PA]): Encoder[When[A, PA]] =
      predEncoder.contramap(_.predicate)
  }

  final case class Always[A, PA <: Predicate[A]]() extends PredicateADT[A](_ => true) {
    override type PredA = PA
  }
  object Always {
    implicit def decoder[A, PA <: Predicate[A]]: Decoder[Always[A, PA]] =
      fixed(true).as(Always[A, PA]()).at("always").or(fixed("any").as(Always[A, PA]()))

    implicit def encoder[A, PA <: Predicate[A]]: Encoder[Always[A, PA]] =
      Encoder.instance(_ => Json.fromString("any"))
  }

  final case class Never[A, PA <: Predicate[A]]() extends PredicateADT[A](_ => false) {
    override type PredA = PA
  }
  object Never {
    implicit def decoder[A, PA <: Predicate[A]]: Decoder[Never[A, PA]] =
      fixed(true).as(Never[A, PA]()).at("never")

    implicit def encoder[A, PA <: Predicate[A]]: Encoder[Never[A, PA]] =
      Encoder.instance(_ => Json.obj("never" := true))
  }

  final case class Is[A, PA <: Predicate[A]](a: A)(implicit E: Eq[A]) extends PredicateADT[A](E.eqv(_, a)) {
    override type PredA = PA
  }
  object Is {
    implicit def decoder[A: Decoder: Eq, PA <: Predicate[A]]: Decoder[Is[A, PA]] =
      Decoder[A].map(Is[A, PA](_)).at("=")

    implicit def encoder[A: Encoder, PA <: Predicate[A]]: Encoder[Is[A, PA]] =
      Encoder.instance(is => Json.obj("=" := is.a))
  }

  final case class In[A, PA <: Predicate[A]](ax: List[A])(implicit E: Eq[A])
    extends PredicateADT[A](a => ax.exists(E.eqv(_, a))) {
    override type PredA = PA
  }
  object In {
    implicit def decoder[A: Decoder : Eq, PA <: Predicate[A]]: Decoder[In[A, PA]] =
      Decoder[List[A]].map(In[A, PA](_)).at("in")

    implicit def encoder[A: Encoder, PA <: Predicate[A]]: Encoder[In[A, PA]] =
      Encoder.instance(in => Json.obj("in" := in.ax))
  }

  final case class LessThan[A, PA <: Predicate[A]](a: A)(implicit O: Order[A]) extends PredicateADT[A](O.lt(_, a)) {
    override type PredA = PA
  }
  object LessThan {
    implicit def decoder[A: Decoder : Order, PA <: Predicate[A]]: Decoder[LessThan[A, PA]] =
      Decoder[A].map(LessThan[A, PA](_)).at("<")

    implicit def encoder[A: Encoder, PA <: Predicate[A]]: Encoder[LessThan[A, PA]] =
      Encoder.instance(lt => Json.obj("<" := lt.a))
  }

  final case class LessThanEq[A, PA <: Predicate[A]](a: A)(implicit O: Order[A])
    extends PredicateADT[A](O.lteqv(_, a)) {
    override type PredA = PA
  }
  object LessThanEq {
    implicit def decoder[A: Decoder : Order, PA <: Predicate[A]]: Decoder[LessThanEq[A, PA]] =
      Decoder[A].map(LessThanEq[A, PA](_)).at("<=")

    implicit def encoder[A: Encoder, PA <: Predicate[A]]: Encoder[LessThanEq[A, PA]] =
      Encoder.instance(lte => Json.obj("<=" := lte.a))
  }

  final case class GreaterThan[A, PA <: Predicate[A]](a: A)(implicit O: Order[A]) extends PredicateADT[A](O.gt(_, a)) {
    override type PredA = PA
  }
  object GreaterThan {
    implicit def decoder[A: Decoder : Order, PA <: Predicate[A]]: Decoder[GreaterThan[A, PA]] =
      Decoder[A].map(GreaterThan[A, PA](_)).at(">")

    implicit def encoder[A: Encoder, PA <: Predicate[A]]: Encoder[GreaterThan[A, PA]] =
      Encoder.instance(gt => Json.obj(">" := gt.a))
  }

  final case class GreaterThanEq[A, PA <: Predicate[A]](a: A)(implicit O: Order[A])
    extends PredicateADT[A](O.gteqv(_, a)) {
    override type PredA = PA
  }
  object GreaterThanEq {
    implicit def decoder[A: Decoder : Order, PA <: Predicate[A]]: Decoder[GreaterThanEq[A, PA]] =
      Decoder[A].map(GreaterThanEq[A, PA](_)).at(">=")

    implicit def encoder[A: Encoder, PA <: Predicate[A]]: Encoder[GreaterThanEq[A, PA]] =
      Encoder.instance(gte => Json.obj(">=" := gte.a))
  }

  final case class Not[A, PA <: Predicate[A]](p: PredicateADT.Aux[A, PA]) extends PredicateADT[A](!p.test(_)) {
    override type PredA = PA
  }
  object Not {
    implicit def decoder[A, PA <: Predicate[A]](implicit D: Decoder[PredicateADT.Aux[A, PA]]): Decoder[Not[A, PA]] =
      D.map(Not[A, PA]).at("!")

    implicit def encoder[A, PA <: Predicate[A]](implicit E: Encoder[PredicateADT.Aux[A, PA]]): Encoder[Not[A, PA]] =
      Encoder.instance(not => Json.obj("!" := not.p))
  }

  final case class ForAll[A, PA <: Predicate[A]](px: List[PredicateADT.Aux[A, PA]])
    extends PredicateADT[A](a => px.forall(_.test(a))) {
    override type PredA = PA
  }
  object ForAll {
    implicit def decoder[A, PA <: Predicate[A]](implicit D: Decoder[PredicateADT.Aux[A, PA]]): Decoder[ForAll[A, PA]] =
      Decoder[List[PredicateADT.Aux[A, PA]]].map(ForAll(_)).at("forall")

    implicit def encoder[A, PA <: Predicate[A]](implicit E: Encoder[PredicateADT.Aux[A, PA]]): Encoder[ForAll[A, PA]] =
      Encoder.instance(forall => Json.obj("forall" := forall.px))
  }

  final case class Exists[A, PA <: Predicate[A]](px: List[PredicateADT.Aux[A, PA]])
    extends PredicateADT[A](a => px.exists(_.test(a))) {
    override type PredA = PA
  }
  object Exists {
    implicit def decoder[A, PA <: Predicate[A]](implicit D: Decoder[PredicateADT.Aux[A, PA]]): Decoder[Exists[A, PA]] =
      Decoder[List[PredicateADT.Aux[A, PA]]].map(Exists(_)).at("exists")

    implicit def encoder[A, PA <: Predicate[A]](implicit E: Encoder[PredicateADT.Aux[A, PA]]): Encoder[Exists[A, PA]] =
      Encoder.instance(exists => Json.obj("exists" := exists.px))
  }
}
