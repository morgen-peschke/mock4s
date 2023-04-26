package peschke.mock4s

import cats.Monad
import cats.syntax.all._
import org.http4s._
import peschke.mock4s.algebras.RequestMatcher
import peschke.mock4s.algebras.ResponseRenderer
import peschke.mock4s.algebras.StateUpdater

object MockApp {
  def default[F[_]: Monad: RequestMatcher: ResponseRenderer: StateUpdater]: HttpApp[F] =
    HttpApp[F] {
      RequestMatcher[F]
        .findResponse(_)
        .flatTap(rd => StateUpdater[F].run(rd.updateState))
        .flatMap(ResponseRenderer[F].render)
    }
}
