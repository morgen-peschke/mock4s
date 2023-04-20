package peschke.mock4s.models

import cats.data.{NonEmptyList, Validated}
import cats.syntax.all._
import com.monovore.decline.Argument
import fs2.io.file.Path

import java.nio.file.InvalidPathException

sealed trait JsonSource
object JsonSource {
  case object ReadStdIn extends JsonSource
  final case class LoadFile(path: Path) extends JsonSource
  final case class LiteralString(string: String) extends JsonSource

  implicit val argument: Argument[JsonSource] =
    Argument.from("json:settings|file:path|-")(_.split(':').toList match {
      case "json" :: rest => LiteralString(rest.mkString(":")).validNel
      case "file" :: rest =>
        val path = rest.mkString(":")
        Validated
          .catchOnly[InvalidPathException](Path(path))
          .bimap(
            e => s"Invalid path <$path>: ${e.getMessage}".pure[NonEmptyList],
            LoadFile
          )
      case "-" :: Nil => ReadStdIn.validNel
      case _ => """Expected Json prefixed with "json:" or a path prefixed with "file:" or "-" for stdin""".invalidNel
    })
}
