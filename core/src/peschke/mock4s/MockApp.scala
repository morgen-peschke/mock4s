package peschke.mock4s

import cats.Monad
import cats.syntax.all._
import org.http4s._
import peschke.mock4s.algebras.{RequestMatcher, ResponseRenderer}

object MockApp {
  def default[F[_]: Monad: RequestMatcher: ResponseRenderer]: HttpApp[F] =
    HttpApp[F](RequestMatcher[F].findResponse(_) >>= ResponseRenderer[F].render)
}
