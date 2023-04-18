package peschke.mock4s

import cats.Monad
import cats.syntax.all._
import org.http4s._
import peschke.mock4s.algebras.{RequestMatcher, ResponseRenderer, StateUpdater}

object MockApp {
  def default[F[_]: Monad: RequestMatcher: ResponseRenderer: StateUpdater]: HttpApp[F] =
    HttpApp[F] {
      RequestMatcher[F]
        .findResponse(_)
        .flatTap(rd => StateUpdater[F].run(rd.updateState))
        .flatMap(ResponseRenderer[F].render)
    }
}
