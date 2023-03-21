package peschke.mock4s

import cats.Monad
import cats.syntax.all._
import org.http4s._
import peschke.mock4s.algebras.{RequestMatcher, ResponseRenderer}

object MockApp {
  def default[F[_]: Monad](requestMatcher: RequestMatcher[F],
                           responseRenderer: ResponseRenderer[F]): HttpApp[F] =
    HttpApp[F](requestMatcher.findResponse(_).flatMap(responseRenderer.render))
}
