package peschke.mock4s.algebras

import cats.syntax.all._
import io.circe.{Json, JsonNumber, JsonObject}
import peschke.mock4s.models.FormatterConfig._

trait Json5Formatter  {
  def format(json: Json): String
}
object Json5Formatter {

  def printer(indent: Indentation,
              sortKeys: SortKeys,
              tightOps: ((TightArrays, TightObjects), (TightCommas, TightColons)),
              singleQuote: SingleQuote
             ): Json5Formatter =
    printer(
      indent,
      sortKeys,
      tightOps._1._1,
      tightOps._1._2,
      tightOps._2._1,
      tightOps._2._2,
      singleQuote
    )

  def printer(indent: Indentation,
              sortKeys: SortKeys,
              tightArrays: TightArrays,
              tightObjects: TightObjects,
              tightCommas: TightCommas,
              tightColons: TightColons,
              singleQuote: SingleQuote
             ): Json5Formatter =
    new Json5Formatter {
      override def format(json: Json): String =
        writeJson(json,
          FormatterState.empty(indent)
        ).builder.result()

      private val openArray = if (tightArrays === TightArrays.Enabled) "[" else "[ "
      private val closeArray = if (tightArrays === TightArrays.Enabled) "]" else " ]"
      private val openObject = if (tightObjects === TightObjects.Enabled) "{" else "{ "
      private val closeObject = if (tightObjects === TightObjects.Enabled) "}" else " }"
      private val colon = if (tightColons === TightColons.Enabled) ":" else ": "
      private val comma = if (tightCommas === TightCommas.Enabled) "," else ", "

      private val arrayBraceWidth = if (tightArrays === TightArrays.Enabled) 1 else 2
      private val objectCurlyWidth = if (tightObjects === TightObjects.Enabled) 1 else 2
      private val commaWidth = if (tightCommas === TightCommas.Enabled) 1 else 2

      private def isScalar(json: Json): Boolean = !json.isArray && !json.isObject

      private def isSimpleArray(value: Vector[Json]): Boolean =
        value.isEmpty ||
          value.forall(isScalar) ||
          (value.length === 1 && value.forall(_.foldWith(isSimpleFolder)))

      private def isSimpleObject(value: JsonObject): Boolean =
        value.isEmpty ||
          (value.size === 1 && value.values.forall(_.foldWith(isSimpleFolder))) ||
          value.values.forall(isScalar)

      private val isSimpleFolder: Json.Folder[Boolean] = new Json.Folder[Boolean] {
        override def onNull: Boolean = true

        override def onBoolean(value: Boolean): Boolean = true

        override def onNumber(value: JsonNumber): Boolean = true

        override def onString(value: String): Boolean = true

        override def onArray(value: Vector[Json]): Boolean = isSimpleArray(value)

        override def onObject(value: JsonObject): Boolean = isSimpleObject(value)
      }

      private def maxWidthReached(lineLength: Int, state: FormatterState): Boolean =
        state.indentation match {
          case Indentation.Never => true
          case Indentation.Always(_) => false
          case Indentation.CollapseSimpleValues(_, maxWidth) => MaxWidth.raw(maxWidth) <= lineLength
        }

      private def arrayOrObjectWouldFitInOneLine
      (parentState: FormatterState, formattedElements: Vector[FormatterState], isArray: Boolean)
      : Boolean = {
        val singleLineLength =
          (2 * (if (isArray) arrayBraceWidth else objectCurlyWidth)) + // Open + Close Braces/Curlies
            formattedElements.foldLeft(parentState.currentIndent)(_ + _.builder.length()) + // Element lengths
            (formattedElements.length * commaWidth) // Commas and spacers
        val shorterThanMaxLength = !maxWidthReached(singleLineLength, parentState)
        val noNewlinesTriggered = formattedElements.forall(!_.isMultiLine)
        shorterThanMaxLength && noNewlinesTriggered
      }

      private def arrayWouldFitInOneLine(parentState: FormatterState, formattedElements: Vector[FormatterState])
      : Boolean =
        arrayOrObjectWouldFitInOneLine(parentState, formattedElements, isArray = true)

      private def objectWouldFitInOneLine
      (parentState: FormatterState, formattedElements: Vector[FormatterState])
      : Boolean =
        arrayOrObjectWouldFitInOneLine(parentState, formattedElements, isArray = false)

      private def commaSeparated(state: FormatterState, elements: Vector[FormatterState]): FormatterState =
        elements match {
          case first +: rest =>
            rest.foldLeft(state.concat(first))(_.append(comma).concat(_))
          case _ => elements.foldLeft(state)(_ concat _)
        }

      private def commaSeparatedIndented(state: FormatterState, elements: Vector[FormatterState]): FormatterState =
        elements match {
          case first +: rest =>
            rest
              .foldLeft(state.append("\n").appendN(first.currentIndent, ' ').concat(first)) { (accum, element) =>
                accum.append(",\n").appendN(element.currentIndent, ' ').concat(element)
              }
          case _ => elements.foldLeft(state)(_ concat _)
        }

      private def writeJson(json: Json, state: FormatterState): FormatterState =
        json.fold(
          jsonNull = writeNull(state),
          jsonBoolean = writeBool(_, state),
          jsonNumber = writeNumber(_, state),
          jsonString = writeString(_, state),
          jsonArray = writeArray(_, state),
          jsonObject = writeObject(_, state)
        )

      private def writeArray(array: Vector[Json], state: FormatterState): FormatterState =
        if (array.isEmpty) state.append("[]")
        else array match {
          case Vector(singleValue) if tightArrays === TightArrays.Enabled && singleValue.isObject =>
            val element = writeJson(singleValue, state.recurse)
            state
              .append('[')
              .concatBuilder(element.builder.reverse.dropWhile(_ === '\n').reverse)
              .append("]")
              .copy(isMultiLine = element.isMultiLine)

          case _ =>
            val elements = array.map(writeJson(_, state.indentAndRecurse))

            def renderAsOneLine = commaSeparated(state.append(openArray), elements).append(closeArray)

            def renderIndented =
              commaSeparatedIndented(state.append("["), elements)
                .append("\n")
                .appendN(state.currentIndent, ' ')
                .append("]")
                .copy(isMultiLine = true)

            state.indentation match {
              case Indentation.Never => renderAsOneLine
              case Indentation.Always(_) => renderIndented
              case Indentation.CollapseSimpleValues(_, _) =>
                if (isSimpleArray(array) && arrayWouldFitInOneLine(state, elements)) renderAsOneLine
                else renderIndented
            }
        }

      private def writeObject(obj: JsonObject, state: FormatterState): FormatterState =
        if (obj.isEmpty) state.append("{}")
        else {
          val elements =
            (if (sortKeys === SortKeys.Enabled) obj.toVector.sortBy(_._1) else obj.toVector)
              .map { case (key, value) =>
                writeJson(value, writeKey(key, state.indentAndRecurse).append(colon))
              }

          def renderAsOneLine = commaSeparated(state.append(openObject), elements).append(closeObject)

          def renderIndented =
            commaSeparatedIndented(state.append("{"), elements)
              .append("\n")
              .appendN(state.currentIndent, ' ')
              .append("}")
              .copy(isMultiLine = true)

          state.indentation match {
            case Indentation.Never => renderAsOneLine
            case Indentation.Always(_) => renderIndented
            case Indentation.CollapseSimpleValues(_, _) =>
              if (isSimpleObject(obj) && objectWouldFitInOneLine(state, elements)) renderAsOneLine
              else renderIndented
          }
        }

      private def writeNull(state: FormatterState): FormatterState =
        state.append("null")

      private def writeBool(value: Boolean, state: FormatterState): FormatterState =
        if (value) state.append("true")
        else state.append("false")

      // cheating a little by peeking into circe internals to be sure this returns a valid value ;)
      private def writeNumber(value: JsonNumber, state: FormatterState): FormatterState =
        state.append(value.toString)

      // Translated from de.marhali.json5.stream.Json5Writer.quote
      private def appendQuoted(value: String, state: FormatterState, quote: Char): FormatterState =
        value.foldLeft(state.append(quote)) { (state, char) =>
          char match {
            case `quote` => state.append('\\').append(quote)
            case '\\' => state.append("\\\\");
            case '\b' => state.append("\\b")
            case '\f' => state.append("\\f")
            case '\n' => state.append("\\n");
            case '\r' => state.append("\\r")
            case '\t' => state.append("\\t")
            case 0x0B => // Vertical Tab
              state.append("\\v")
            case _ =>
              // escape non-graphical characters (https://www.unicode.org/versions/Unicode13.0.0/ch02.pdf#G286941)
              Character.getType(char) match {
                case Character.FORMAT |
                     Character.LINE_SEPARATOR |
                     Character.PARAGRAPH_SEPARATOR |
                     Character.CONTROL |
                     Character.PRIVATE_USE |
                     Character.SURROGATE |
                     Character.UNASSIGNED => state.append(String.format("\\u%04X", char.toInt));
                case _ => state.append(char)
              }
          }
        }.append(quote)

      private def writeKey(value: String, state: FormatterState): FormatterState =
        appendQuoted(
          value,
          state,
          singleQuote match {
            case SingleQuote.Disabled => '"'
            case SingleQuote.KeysOnly | SingleQuote.Always => '\''
            case SingleQuote.MinimizeEscaping =>
              val singleCount = value.count(_ === '\'')
              val doubleCount = value.count(_ === '"')
              if (singleCount <= doubleCount) '\''
              else '"'
          }
        )

      // Translated from de.marhali.json5.stream.Json5Writer.quote
      private def writeString(value: String, state: FormatterState): FormatterState =
        appendQuoted(
          value,
          state,
          singleQuote match {
            case SingleQuote.Disabled | SingleQuote.KeysOnly => '"'
            case SingleQuote.Always => '\''
            case SingleQuote.MinimizeEscaping =>
              val singleCount = value.count(_ === '\'')
              val doubleCount = value.count(_ === '"')
              if (singleCount < doubleCount) '\''
              else '"'
          }
        )
    }

  private final case class FormatterState
    (builder: StringBuilder, currentIndent: Int, indentation: Indentation, isMultiLine: Boolean) {

    def append(char: Char): FormatterState = {
      builder.append(char)
      this
    }

    def append(str: String): FormatterState = {
      builder.appendAll(str)
      this
    }

    def appendN(count: Int, c: Char): FormatterState = {
      builder.appendAll(Array.fill(count)(c))
      this
    }

    def concatBuilder(other: StringBuilder): FormatterState = {
      builder.append(other)
      this
    }

    def concat(other: FormatterState): FormatterState = concatBuilder(other.builder)

    def recurse: FormatterState = FormatterState(
      builder = new StringBuilder(),
      currentIndent = currentIndent,
      indentation = indentation,
      isMultiLine = false
    )

    def indentAndRecurse: FormatterState = FormatterState(
      builder = new StringBuilder(),
      currentIndent = indentation match {
        case Indentation.Never => currentIndent
        case Indentation.Always(delta) => currentIndent + Delta.raw(delta)
        case Indentation.CollapseSimpleValues(delta, _) => currentIndent + Delta.raw(delta)
      },
      indentation = indentation,
      isMultiLine = false
    )
  }

  private object FormatterState {
    def empty(indent: Indentation): FormatterState = FormatterState(new StringBuilder(), 0, indent, isMultiLine = false)
  }
}
