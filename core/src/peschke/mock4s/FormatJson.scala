package peschke.mock4s

import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.std.Console
import cats.syntax.all._
import com.monovore.decline.{Argument, Command, Opts}
import io.circe.Json
import peschke.mock4s.algebras.{Json5Formatter, JsonFormatter, JsonSourceResolver}
import peschke.mock4s.models.FormatterConfig._
import peschke.mock4s.models.JsonSource

object FormatJson extends IOApp {

  private val sourceOpt: Opts[JsonSource] = Opts.argument[JsonSource]()

  private val indentOpt: Opts[Delta] =
    Opts
      .option[Int](
        long = "indent",
        help = "The amount to increase the indent at each level of nesting (default: 2)"
      )
      .orElse(2.pure[Opts])
      .map(Delta(_))

  private val sortKeysOpt: Opts[SortKeys] =
    Opts
      .flag(long = "sort-keys", help = "Sort keys of JSON objects (default: false)")
      .orFalse
      .map(SortKeys(_))

  private val maxWidthOpt: Opts[MaxWidth] =
    Opts
      .option[Int](short = "w", long = "max-width", help = "Best-effort max line length (default: 120)")
      .orElse(120.pure[Opts])
      .map(MaxWidth(_))

  private val tightArraysOpt: Opts[TightArrays] =
    Opts
      .flag(
        long = "tight-arrays",
        help = "Do not include a space before opening and closing a compact array. Ex '[{}]' vs '[ {} ]'"
      )
      .orFalse
      .map(TightArrays(_))

  private val tightObjectsOpt: Opts[TightObjects] =
    Opts
      .flag(
        long = "tight-objects",
        help =
          """Do not include a space before opening and closing a compact object. Ex '{"key":1}' vs '{ "key":1 }'"""
      )
      .orFalse
      .map(TightObjects(_))

  private val tightBracesOpt: Opts[(TightArrays, TightObjects)] =
    Opts
      .flag(long = "tight-braces", help = "Alias for both --tight-arrays and --tight-objects")
      .as(TightArrays.Enabled -> TightObjects.Enabled)
      .orElse((tightArraysOpt, tightObjectsOpt).tupled)

  private val jsonFormatterOpt: Opts[(JsonFormatter, JsonSource)] = {
    val compactCmd =
      Opts.subcommand[(JsonFormatter, JsonSource)](
        name = "compact",
        help = "Format as standard JSON, on a single line"
      )(JsonFormatter.compact.pure[Opts].product(sourceOpt))

    val prettyCmd =
      Opts.subcommand[(JsonFormatter, JsonSource)](
        name = "pretty",
        help = "Format as standard JSON, usually nicely indented"
      )((indentOpt, sortKeysOpt).mapN(JsonFormatter.pretty).product(sourceOpt))

    val terseCmd =
      Opts.subcommand[(JsonFormatter, JsonSource)](
        name = "terse",
        help =
          """|Format as standard JSON, attempting to minimize horizontal size by rendering
             |simple parts of JSON in the compact style, while retaining readability by
             |rendering more complex parts in an indented style""".stripMargin
      ) {
        (indentOpt, sortKeysOpt, maxWidthOpt, tightBracesOpt)
          .mapN(JsonFormatter.terse)
          .product(sourceOpt)
      }

    compactCmd.orElse(prettyCmd).orElse(terseCmd)
  }

  private val json5FormatterOpt: Opts[(Json5Formatter, JsonSource)] = {
    val tightCommasOpt: Opts[TightCommas] =
      Opts
        .flag(
          long = "tight-commas",
          help = "Do not include a space after commas in compact array and objects. Ex '[1,2]' vs '[1, 2]'"
        )
        .orFalse
        .map(TightCommas(_))

    val tightColonsOpt: Opts[TightColons] =
      Opts
        .flag(
          long = "tight-commas",
          help = """Do not include a space after colons in compact objects. Ex '{"a":1}' vs '{"a": 1}'"""
        )
        .orFalse
        .map(TightColons(_))

    val tightDelimOpt: Opts[(TightCommas, TightColons)] =
      Opts
        .flag(long = "tight-delim", help = "Alias for both --tight-commas and --tight-colons")
        .as(TightCommas.Enabled -> TightColons.Enabled)
        .orElse((tightCommasOpt, tightColonsOpt).tupled)

    val tightOpts: Opts[((TightArrays, TightObjects), (TightCommas, TightColons))] =
      Opts
        .flag(long = "tight", help = "Alias for enabling all --tight-* flags")
        .as((TightArrays.Enabled -> TightObjects.Enabled) -> (TightCommas.Enabled -> TightColons.Enabled))
        .orElse((tightBracesOpt, tightDelimOpt).tupled)

    implicit val singleQuoteArg: Argument[SingleQuote] = Argument.fromMap[SingleQuote](
      "quote-style",
      Map(
        "disabled" -> SingleQuote.Disabled,
        "keys-only" -> SingleQuote.KeysOnly,
        "always" -> SingleQuote.Always,
        "minimize-escaping" -> SingleQuote.MinimizeEscaping
      )
    )

    val singleQuoteOpt =
      Opts.option[SingleQuote](
        long = "single-quote-style",
        help =
          """|Set the single-quote handling style.
             |  disabled          : Never use single quotes instead of double quotes
             |  keys-only         : Always use single quotes for object keys, and don't use elsewhere
             |  always            : Always use single quotes instead of double quotes
             |  minimize-escaping : Choose whichever quote style for a particular key or value that will
             |                      result in the fewest escaped characters in the result.""".stripMargin
      )

    val compactCmd =
      Opts.subcommand[(Json5Formatter, JsonSource)](
        name = "compact5",
        help =
          raw"""|Format as JSON5, as compactly as possible. Equivalent to:
                |  json5 \
                |    --no-indent \
                |    --tight-arrays \
                |    --tight-objects \
                |    --tight-commas \
                |    --tight-colons \
                |    --single-quote-style minimize-escaping""".stripMargin
      )(Json5Formatter.printer(
        Indentation.Never,
        SortKeys.Disabled,
        TightArrays.Enabled,
        TightObjects.Enabled,
        TightCommas.Enabled,
        TightColons.Enabled,
        SingleQuote.MinimizeEscaping
      ).pure[Opts].product(sourceOpt))

    val oneLineCmd =
      Opts.subcommand[(Json5Formatter, JsonSource)](
        name = "oneLine5",
        help =
          """|Format as JSON5, on a single line. Equivalent to:
             |  json5 --no-indent""".stripMargin
      )(Json5Formatter.printer(
        Indentation.Never,
        SortKeys.Disabled,
        TightArrays.Disabled,
        TightObjects.Disabled,
        TightCommas.Disabled,
        TightColons.Disabled,
        SingleQuote.Disabled
      ).pure[Opts].product(sourceOpt))

    val prettyCmd =
      Opts.subcommand[(Json5Formatter, JsonSource)](
        name = "pretty5",
        help =
          raw"""|Format as JSON5, nicely indented. Equivalent to:
                |  json5 \
                |    --always-indent <delta> \
                |    --single-quote-style minimize-escaping \
                |    [--sort-keys]""".stripMargin
      ) {
        (indentOpt, sortKeysOpt).mapN { (delta, sortKeys) =>
          Json5Formatter.printer(
            Indentation.Always(delta),
            sortKeys,
            TightArrays.Disabled,
            TightObjects.Disabled,
            TightCommas.Disabled,
            TightColons.Disabled,
            SingleQuote.MinimizeEscaping
          )
        }.product(sourceOpt)
      }

    val terseCmd =
      Opts.subcommand[(Json5Formatter, JsonSource)](
        name = "terse5",
        help =
          """|Format using JSON5 conventions, attempting to minimize horizontal size by rendering
             |simple parts of JSON in the compact style, while retaining readability by rendering
             |more complex parts in an indented style.
             |
             |Equivalent to:
             |  json5 \
             |    --smart-indent \
             |    --indent-delta <delta> \
             |    --max-width <width> \
             |    --single-quote-style minimize-escaping \
             |    [--sort-keys] \
             |    [--tight-arrays] \
             |    [--tight-objects] \
             |    [--tight-commas] \
             |    [--tight-colons]
             |""".stripMargin
      ) {
        (indentOpt, sortKeysOpt, maxWidthOpt, tightOpts)
          .mapN { (indent, sortKeys, maxWidth, tight) =>
            Json5Formatter.printer(
              Indentation.CollapseSimpleValues(indent, maxWidth),
              sortKeys,
              tight,
              SingleQuote.MinimizeEscaping
            )
          }
          .product(sourceOpt)
      }

    val json5Cmd =
      Opts.subcommand[(Json5Formatter, JsonSource)](
        name = "json5",
        help = "Format using JSON5 convention"
      ) {
        val neverIndentOpt: Opts[Indentation] =
          Opts
            .flag("no-indent", help = "Disable indentation for arrays and objects")
            .as(Indentation.Never)

        val alwaysIndentOpt: Opts[Indentation] =
          Opts
            .flag("always-indent", help = "Always indent arrays and objects")
            .productR(indentOpt)
            .map(Indentation.Always)

        val smartIndentOpt: Opts[Indentation] =
          Opts
            .flag("smart-indent", help = "Always indent arrays and objects")
            .productR((indentOpt, maxWidthOpt).mapN(Indentation.CollapseSimpleValues))

        val indentationOpt = neverIndentOpt.orElse(alwaysIndentOpt).orElse(smartIndentOpt)

        (indentationOpt, sortKeysOpt, tightOpts, singleQuoteOpt)
          .mapN(Json5Formatter.printer)
          .product(sourceOpt)
      }

    compactCmd.orElse(oneLineCmd).orElse(prettyCmd).orElse(terseCmd).orElse(json5Cmd)
  }

  private val command =
    Command(name = "FormatJson", header = "JSON/JSON5 formatting utility") {
      jsonFormatterOpt.sum(json5FormatterOpt)
    }.map(_.fold(
      t => t._1.asLeft -> t._2,
      t => t._1.asRight -> t._2
    ))

  @SuppressWarnings(Array("scalafix:DisableSyntax.noPrintln"))
  override def run(args: List[String]): IO[ExitCode] = {
    val jsr = JsonSourceResolver.default[IO]

    command
      .parse(args).fold(
        Console[IO].errorln(_).as(ExitCode.Error),
        { case (formatter, source) =>
          jsr
            .resolve[Json](source)
            .map(json => formatter.fold(_.format(json), _.format(json)))
            .flatMap(Console[IO].println(_))
            .as(ExitCode.Success)
        }
      )

  }
}
