package peschke.mock4s

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.std.Console
import cats.syntax.all._
import com.monovore.decline.Command
import com.monovore.decline.Opts
import io.circe.Json
import peschke.mock4s.algebras.JsonFormatter
import peschke.mock4s.algebras.JsonFormatter.IndentDelta
import peschke.mock4s.algebras.JsonFormatter.MaxWidth
import peschke.mock4s.algebras.JsonFormatter.TightArrays
import peschke.mock4s.algebras.JsonFormatter.TightObjects
import peschke.mock4s.algebras.JsonSourceResolver
import peschke.mock4s.models.JsonSource

object FormatJson extends IOApp {

  private val sourceOpt = Opts.argument[JsonSource]()

  private val indentOpt: Opts[IndentDelta] =
    Opts
      .option[Int](
        short = "i",
        long = "indent",
        help = "The amount to increase the indent at each level of nesting (default: 2)"
      )
      .orElse(2.pure[Opts])
      .map(IndentDelta(_))

  private val sortKeysOpt =
    Opts.flag(long = "sort-keys", help = "Sort keys of JSON objects (default: false)").orFalse

  private val compactFormatterOpt =
    Opts
      .subcommand[(JsonFormatter, JsonSource)](
        name = "compact",
        help = "Format using Circe's noSpaces formatter"
      )(JsonFormatter.compact.pure[Opts].product(sourceOpt))

  private val prettyFormatterOpt =
    Opts.subcommand[(JsonFormatter, JsonSource)](
      name = "pretty",
      help = "Format using Circe's pretty formatter"
    )((indentOpt, sortKeysOpt, sourceOpt).mapN(JsonFormatter.pretty(_, _) -> _))

  private val terseFormatterOpt =
    Opts.subcommand[(JsonFormatter, JsonSource)](
      name = "terse",
      help = """|Format attempting to minimize horizontal size by rendering simple parts of JSON in the compact style,
                 |while retaining readability by rendering more complex parts in an indented style""".stripMargin
    ) {
      val maxWidthOpt: Opts[MaxWidth] =
        Opts
          .option[Int](short = "w", long = "max-width", help = "Best-effort max line length (default: 120)")
          .orElse(120.pure[Opts])
          .map(MaxWidth(_))

      val tightArraysOpt: Opts[TightArrays] =
        Opts
          .flag(
            long = "tight-arrays",
            help = "Do not include a space before opening and closing a compact array. Ex '[{}]' vs '[ {} ]'"
          )
          .orFalse
          .map(TightArrays(_))

      val tightObjectsOpt: Opts[TightObjects] =
        Opts
          .flag(
            long = "tight-objects",
            help =
              """Do not include a space before opening and closing a compact object. Ex '{"key":1}' vs '{ "key":1 }'"""
          )
          .orFalse
          .map(TightObjects(_))

      val tightOpt: Opts[(TightArrays, TightObjects)] =
        Opts
          .flag(long = "tight", help = "Alias for both --tight-arrays and --tight-objects")
          .as(TightArrays(true) -> TightObjects(true))

      (indentOpt, sortKeysOpt, maxWidthOpt, tightOpt.orElse((tightArraysOpt, tightObjectsOpt).tupled), sourceOpt)
        .mapN { (indent, sortKeys, maxWidth, tightFlags, source) =>
          JsonFormatter.terse(indent, sortKeys, maxWidth, tightFlags._1, tightFlags._2) -> source
        }
    }

  private val formatterOpt = compactFormatterOpt.orElse(prettyFormatterOpt).orElse(terseFormatterOpt)

  private val command =
    Command(name = "FormatJson", header = "JSON formatting utility")(formatterOpt)

  @SuppressWarnings(Array("scalafix:DisableSyntax.noPrintln"))
  override def run(args: List[String]): IO[ExitCode] = {
    val jsr = JsonSourceResolver.default[IO]

    command
      .parse(args).fold(
        Console[IO].errorln(_).as(ExitCode.Error),
        { case (formatter, source) =>
          jsr
            .resolve[Json](source)
            .map(formatter.format)
            .flatMap(Console[IO].println(_))
            .as(ExitCode.Success)
        }
      )

  }
}
