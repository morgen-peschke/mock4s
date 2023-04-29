package peschke.mock4s.algebras

import cats.Functor
import cats.Monad
import cats.Show
import cats.data.Chain
import cats.data.NonEmptyChain
import cats.data.Validated
import cats.syntax.all._
import peschke.mock4s.algebras.Parser.ParseError
import peschke.mock4s.algebras.Parser.ParserName
import peschke.mock4s.algebras.Parser.State
import peschke.mock4s.algebras.Parser.State.History

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq

trait Parser[Output] { self =>
  def name: ParserName

  final def parseE(input: String): ParseError.OrRight[Output] =
    parseE(Chain.fromSeq(ArraySeq.unsafeWrapArray(input.toCharArray)))

  final def parseV(input: String): ParseError.OrValid[Output] =
    parseV(Chain.fromSeq(ArraySeq.unsafeWrapArray(input.toCharArray)))

  final def parseE(input: Chain[Char]): ParseError.OrRight[Output] = parseE(Parser.init(input))
  final def parseV(input: Chain[Char]): ParseError.OrValid[Output] = parseV(Parser.init(input))

  final def parseE(state: State): ParseError.OrRight[Output] = attemptE(state.addParser(name))
  final def parseV(state: State): ParseError.OrValid[Output] = attemptV(state.addParser(name))

  def attemptE(state: State): ParseError.OrRight[Output]
  def attemptV(state: State): ParseError.OrValid[Output]

  def withName(newName: String): Parser[Output] =
    new Parser[Output] {
      override val name: ParserName = ParserName(newName)
      override def attemptE(state: State): ParseError.OrRight[Output] = self.attemptE(state)
      override def attemptV(state: State): ParseError.OrValid[Output] = self.attemptV(state)
    }

  def withEnclosingName(implicit newName: sourcecode.Name): Parser[Output] = withName(newName.value)

  def map[O](f: Output => O): Parser[O] =
    Parser.namedInstance[O](show"$name.map(...)") { input =>
      self.parseE(input).map(_.map(f))
    }

  def flatMap[O](f: Output => Parser[O]): Parser[O] =
    Parser.namedInstance[O](show"$name.flatMap(...)") { input =>
      self.parseE(input).flatMap { output =>
        f(output.value).parseE(input)
      }
    }

  def repeated: Parser[Chain[Output]] =
    Parser.namedInstance[Chain[Output]](show"$name.repeated") { input =>
      @tailrec
      def loop(state: State, accum: Chain[Output]): Parser.Result[Chain[Output]] =
        self.parseE(state.usingHistory(input)) match {
          case Left(_)                               => Parser.Result(accum, state)
          case Right(Parser.Result(value, newState)) => loop(newState, accum.append(value))
        }

      loop(input, Chain.empty).asRight
    }

  def repeatedNec: Parser[NonEmptyChain[Output]] =
    (self ~> self.repeated).map { case (first, rest) =>
      NonEmptyChain.fromChainPrepend(first, rest)
    }

  def ? : Parser[Option[Output]] =
    Parser.namedInstance(show"($name)?") { input =>
      self.parseE(input) match {
        case Left(_) => Parser.Result(none[Output], input).asRight
        case Right(value) => value.map(_.some).asRight
      }
    }

  def ~>[O2](next: Parser[O2]): Parser[(Output, O2)] =
    Parser.namedInstance(show"$name ~> ${next.name}") { input =>
      for {
        first  <- self.parseE(input)
        second <- next.parseE(first.state)
      } yield Parser.Result(first.value -> second.value, second.state)
    }

  def ~+:>[O2 <: Output](next: Parser[Chain[Output]]): Parser[NonEmptyChain[Output]] =
    (self ~> next)
      .map { case (single, chain) =>
        NonEmptyChain.fromChainPrepend(single, chain)
      }
      .withName(s"$name ~+:> ${next.name}")

  def *>[O2](next: Parser[O2]): Parser[O2] =
    Parser.namedInstance(show"$name *> ${next.name}") { input =>
      for {
        first  <- self.parseE(input)
        second <- next.parseE(first.state)
      } yield Parser.Result(second.value, second.state)
    }

  def <*[O2](next: Parser[O2]): Parser[Output] =
    Parser.namedInstance(show"$name <* ${next.name}") { input =>
      for {
        first  <- self.parseE(input)
        second <- next.parseE(first.state)
      } yield Parser.Result(first.value, second.state)
    }
}
object Parser        {
  final case class State(value: Chain[Char], index: Int, history: Chain[History]) {
    def uncons: Option[(Char, State)] =
      value.uncons.map { case (char, rest) =>
        char -> State(rest, index + 1, history)
      }

    def isEmpty: Boolean = value.isEmpty

    def as(value: Chain[Char]): State = State(value, index, history)

    def addParser(name: ParserName): State = State(value, index, history.append(History(index, name, value)))

    def usingHistory(other: State): State = State(value, index, other.history)

    def render: String = {
      val valueStr = value.mkString_("Chain(", ",", ")")
      val historyStr = history.mkString_("Chain(", ",", ")")
      s"State($valueStr, $index, $historyStr)"
    }
  }
  object State                                                                    {
    final case class History(index: Int, parser: ParserName, input: Chain[Char]) {
      def truncatedInput(length: Int): String =
        if (input.length <= length) input.mkString_("")
        else input.iterator.take(length).mkString("", "", "...")
    }
    implicit val showHistory: Show[History] = Show.show { h =>
      val input =
        if (h.input.length <= 20) h.input.mkString_("")
        else h.input.iterator.take(20).mkString("", "", "...")
      s"${h.index} :: ${h.parser} << $input"
    }
    implicit val show: Show[State] = Show.show(_.render)
  }

  final case class Result[Output](value: Output, state: State) {
    def map[O](f: Output => O): Result[O] = Result[O](f(value), state)
  }
  object Result                                                {
    def valid[Output](value: Output, state: State): ParseError.OrValid[Output] =
      Result(value, state).valid

    def right[Output](value: Output, state: State): ParseError.OrRight[Output] =
      Result(value, state).asRight

    implicit val catsInstances: Functor[Result] =
      new Functor[Result] {
        override def map[A, B](fa: Result[A])(f: A => B): Result[B] = fa.map(f)
      }
  }

  object ErrorMessage extends supertagged.NewType[String] {
    implicit val show: Show[Type] = Show.show(raw)
  }
  type ErrorMessage = ErrorMessage.Type

  object ParserName extends supertagged.NewType[String] {
    implicit val show: Show[Type] = Show.show(raw)
  }
  type ParserName = ParserName.Type

  final case class ParseError(message: ErrorMessage, state: State) {
    override def toString: String = show"ParseError($message, $state)"
  }
  object ParseError                                                {
    type OrRight[Output] = Either[NonEmptyChain[ParseError], Result[Output]]
    type OrValid[Output] = Validated[NonEmptyChain[ParseError], Result[Output]]

    def one(message: String, state: State): NonEmptyChain[ParseError] =
      NonEmptyChain.one(ParseError(ErrorMessage(message), state))

    def of(state: State)(firstMessage: String, secondMessage: String, messages: String*): NonEmptyChain[ParseError] =
      NonEmptyChain
        .fromChainPrepend(firstMessage, Chain.fromSeq(messages).prepend(secondMessage))
        .map(m => ParseError(ErrorMessage(m), state))

    implicit def show: Show[ParseError] = Show.show { case ParseError(message, State(_, index, history)) =>
      val maxIndexWidth = (history.map(_.index).maximumOption.getOrElse(0) % 10).abs.max(1)
      val maxParserNameWidth = history.map(h => ParserName.raw(h.parser).length).maximumOption.getOrElse(0).max(1)
      val historyStr =
        history
          .map { h =>
            s"%${maxIndexWidth}d :: %-${maxParserNameWidth}s << %s".format(
              h.index,
              h.parser,
              h.truncatedInput(20)
            )
          }.distinct.mkString_("\n", "\n", "\n")
      show"Parse error at index $index, $message:$historyStr"
    }
  }

  def init(input: Chain[Char]): State = State(input, 0, Chain.empty)

  def instance[Output](parseFn: State => ParseError.OrRight[Output])(implicit name: sourcecode.Name): Parser[Output] =
    namedInstance[Output](name.value)(parseFn)

  def namedInstance[Output](parserName: String)(parseFn: State => ParseError.OrRight[Output]): Parser[Output] =
    new Parser[Output] {
      override val name: ParserName = ParserName(parserName)

      override def attemptE(state: State): ParseError.OrRight[Output] = parseFn(state)

      override def attemptV(state: State): ParseError.OrValid[Output] = parseE(state).toValidated
    }

  def accumulating[Output](parseFn: State => ParseError.OrValid[Output])(implicit name: sourcecode.Name): Parser[Output] =
    namedAccumulating[Output](name.value)(parseFn)

  def namedAccumulating[Output](parserName: String)(parseFn: State => ParseError.OrValid[Output]): Parser[Output] =
    new Parser[Output] {
      override val name: ParserName = ParserName(parserName)

      override def attemptE(state: State): ParseError.OrRight[Output] = parseV(state).toEither

      override def attemptV(state: State): ParseError.OrValid[Output] = parseFn(state)
    }

  val noOp: Parser[Unit] = Parser.instance(input => Result.right((), input))

  val End: Parser[Unit] = Parser.instance { input =>
    if (input.isEmpty) Result.right((), input)
    else ParseError.one("expected end of input", input).asLeft
  }

  def fixed(sentinel: Char): Parser[Char] =
    Parser.namedAccumulating(s"fixed($sentinel)") { input =>
      input.uncons match {
        case Some(head -> state) if head === sentinel => Result(head, state).valid
        case Some(head -> _)                          =>
          ParseError.one(show"expected <$sentinel>, but was <$head>", input).invalid
        case None                                     =>
          ParseError.one(show"expected <$sentinel>, but ran out of input", input).invalid
      }
    }

  def fixed(sentinel: String): Parser[String] =
    sentinel
      .toVector.map(fixed)
      .reduceLeftOption(_ *> _)
      .getOrElse(noOp)
      .as(sentinel)
      .withName(s"fixed($sentinel)")

  def findValid[Output](p0: Parser[Output], pN: Parser[Output]*): Parser[Output] = {
    val pchain = Chain.fromSeq(pN)
    val name = pchain.prepend(p0).map(_.name).mkString_("findValid(", ",", ")")
    Parser.namedAccumulating[Output](name) { input =>
      pchain.foldLeft(p0.parseV(input)) { (r, p) =>
        r.findValid(p.parseV(input))
      }
    }
  }

  implicit final class ChainParserOps[Output](private val parser: Parser[Chain[Output]]) extends AnyVal {
    def ~++>[O2 <: Output](next: Parser[Chain[O2]]): Parser[Chain[Output]] = {
      (parser ~> next)
        .map(t => t._1 ++ t._2)
        .withName(s"${parser.name} ~++> ${next.name}")
    }
  }

  implicit final class NonEmptyChainParserOps[Output](private val parser: Parser[NonEmptyChain[Output]]) extends AnyVal {
    def ~++>[O2 <: Output](next: Parser[Chain[O2]]): Parser[NonEmptyChain[Output]] = {
      (parser ~> next)
        .map(t => t._1.appendChain(t._2))
        .withName(s"${parser.name} ~++> ${next.name}")
    }
  }

  implicit final class OptParserOps[Output](private val parser: Parser[Option[Output]]) extends AnyVal {
    def force: Parser[Output] =
      Parser.namedInstance[Output](show"(${parser.name})!") { input =>
        parser.attemptE(input).flatMap { result =>
          result.value match {
            case Some(value) => Parser.Result(value, result.state).asRight
            case None => ParseError.one(s"expected ${parser.name} to succeed at least once", input).asLeft
          }
        }
      }

    def repeatWhileSome: Parser[Chain[Output]] =
      Parser.namedInstance[Chain[Output]](show"${parser.name}.repeatWhileSome") { input =>
        @tailrec
        def loop(state: State, accum: Chain[Output]): Parser.Result[Chain[Output]] =
          parser.parseE(state) match {
            case Left(_)                                     => Parser.Result(accum, state)
            case Right(Parser.Result(Some(value), newState)) => loop(newState, accum.append(value))
            case Right(Parser.Result(None, _))               => Parser.Result(accum, state)
          }

        loop(input, Chain.empty).asRight
      }

    def repeatWhileSomeNec: Parser[NonEmptyChain[Output]] =
      (parser.force ~+:> parser.repeatWhileSome).withName("repeatWhileSomeNec")
  }

  implicit def catsInstances: Monad[Parser[*]] = new Monad[Parser[*]] {
    override def flatMap[A, B](fa: Parser[A])(f: A => Parser[B]): Parser[B] = fa.flatMap(f)

    override def map[A, B](fa: Parser[A])(f: A => B): Parser[B] = fa.map(f)

    override def tailRecM[A, B](a: A)(f: A => Parser[Either[A, B]]): Parser[B] =
      Parser.namedInstance[B]("tailRecM") { input =>
        @tailrec
        def loop(current: A): ParseError.OrRight[B] =
          f(current).parseE(input) match {
            case Left(errors)                  => errors.asLeft
            case Right(Result(Right(b), s))    => Result(b, s).asRight
            case Right(Result(Left(nextA), _)) => loop(nextA)
          }
        loop(a)
      }

    override def pure[A](x: A): Parser[A] = Parser.namedInstance[A](s"pure($x)")(Result(x, _).asRight)
  }
}
