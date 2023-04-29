package peschke.mock4s.algebras

import cats.syntax.all._
import io.circe.Json
import io.circe.JsonNumber
import io.circe.JsonObject
import io.circe.Printer
import peschke.mock4s.models.FormatterConfig.{Delta, MaxWidth, SortKeys, TightArrays, TightObjects}

import scala.annotation.switch

trait JsonFormatter  {
  def format(json: Json): String
}
object JsonFormatter {
  val compact: JsonFormatter = new UsingCircePrinter(Printer.noSpaces)

  def pretty(indent: Delta, sortKeys: SortKeys): JsonFormatter =
    new UsingCircePrinter(Printer.indented(" " * Delta.raw(indent), SortKeys.raw(sortKeys)))

  private final class UsingCircePrinter(printer: Printer) extends JsonFormatter {
    override def format(json: Json): String = json.printWith(printer)
  }

  def terse(indent: Delta, sortKeys: SortKeys, maxWidth: MaxWidth, tightDelims: (TightArrays, TightObjects))
  : JsonFormatter = terse(indent, sortKeys, maxWidth, tightDelims._1, tightDelims._2)

  def terse
    (indent: Delta, sortKeys: SortKeys, maxWidth: MaxWidth, tightArrays: TightArrays, tightObjects: TightObjects)
    : JsonFormatter =
    new JsonFormatter {
      override def format(json: Json): String =
        writeJson(json, FormatterState.empty(maxWidth)).builder.result()

      private val openArray = if (tightArrays === TightArrays.Enabled) "[" else "[ "
      private val closeArray = if (tightArrays === TightArrays.Enabled) "]" else " ]"
      private val openObject = if (tightObjects === TightObjects.Enabled) "{" else "{ "
      private val closeObject = if (tightObjects === TightObjects.Enabled) "}" else " }"

      private val arrayBraceWidth = if (tightArrays === TightArrays.Enabled) 1 else 2
      private val objectCurlyWidth = if (tightObjects === TightObjects.Enabled) 1 else 2

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

      private def arrayOrObjectWouldFitInOneLine
        (parentState: FormatterState, formattedElements: Vector[FormatterState], isArray: Boolean)
        : Boolean = {
        val maxLengthReached =
          (2 * (if (isArray) arrayBraceWidth else objectCurlyWidth)) +                      // Open + Close Braces/Curlies
            formattedElements.foldLeft(parentState.currentIndent)(_ + _.builder.length()) + // Element lengths
            (formattedElements.length * 2)                                                  // Commas and spacers
        val shorterThanMaxLength = maxLengthReached < MaxWidth.raw(parentState.maxWidth)
        val noNewlinesTriggered = formattedElements.forall(!_.isMultiLine)
        shorterThanMaxLength && noNewlinesTriggered
      }

      private def arrayWouldFitInOneLine
        (parentState: FormatterState, formattedElements: Vector[FormatterState])
        : Boolean =
        arrayOrObjectWouldFitInOneLine(parentState, formattedElements, isArray = true)

      private def objectWouldFitInOneLine
        (parentState: FormatterState, formattedElements: Vector[FormatterState])
        : Boolean =
        arrayOrObjectWouldFitInOneLine(parentState, formattedElements, isArray = false)

      private def commaSeparated(state: FormatterState, elements: Vector[FormatterState]): FormatterState =
        elements match {
          case first +: rest =>
            rest.foldLeft(state.concat(first))(_.append(", ").concat(_))
          case _             => elements.foldLeft(state)(_ concat _)
        }

      private def commaSeparatedIndented(state: FormatterState, elements: Vector[FormatterState]): FormatterState =
        elements match {
          case first +: rest =>
            rest.foldLeft(state.append("\n").appendN(first.currentIndent, ' ').concat(first)) { (accum, element) =>
              accum.append(",\n").appendN(element.currentIndent, ' ').concat(element)
            }
          case _             => elements.foldLeft(state)(_ concat _)
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
        else {
          val elements = array.map(writeJson(_, state.indentAndRecurse(indent)))
          if (isSimpleArray(array) && arrayWouldFitInOneLine(state, elements))
            commaSeparated(state.append(openArray), elements).append(closeArray)
          else
            commaSeparatedIndented(state.append("["), elements)
              .append("\n")
              .appendN(state.currentIndent, ' ')
              .append("]")
              .copy(isMultiLine = true)
        }

      private def writeObject(obj: JsonObject, state: FormatterState): FormatterState =
        if (obj.isEmpty) state.append("{}")
        else {
          val elements = (if (sortKeys === SortKeys.Enabled) obj.toVector.sortBy(_._1) else obj.toVector).map { case (key, value) =>
            val entryState = state.indentAndRecurse(indent)
            writeString(key, entryState)
            entryState.append(": ")
            writeJson(value, entryState)
          }
          if (isSimpleObject(obj) && objectWouldFitInOneLine(state, elements))
            commaSeparated(state.append(openObject), elements).append(closeObject)
          else
            commaSeparatedIndented(state.append("{"), elements)
              .append("\n")
              .appendN(state.currentIndent, ' ')
              .append("}")
              .copy(isMultiLine = true)
        }

      private def writeNull(state: FormatterState): FormatterState =
        state.append("null")

      private def writeBool(value: Boolean, state: FormatterState): FormatterState =
        if (value) state.append("true")
        else state.append("false")

      // cheating a little by peeking into circe internals to be sure this returns a valid value ;)
      private def writeNumber(value: JsonNumber, state: FormatterState): FormatterState =
        state.append(value.toString)

      // Taken nearly verbatim from io.circe.Printer.PrintingFolder#onString
      @SuppressWarnings(Array(
        "scalafix:DisableSyntax.var",
        "scalafix:DisableSyntax.while"
      ))
      private def writeString(value: String, state: FormatterState): FormatterState = {
        def toHex(nibble: Int): Char = (nibble + (if (nibble >= 10) 87 else 48)).toChar

        val builder = state.builder.underlying
        builder.append('"')

        var i = 0
        var offset = 0

        while (i < value.length) {
          val c = value.charAt(i)

          val esc = (c: @switch) match {
            case '"'  => '"'
            case '\\' => '\\'
            case '\b' => 'b'
            case '\f' => 'f'
            case '\n' => 'n'
            case '\r' => 'r'
            case '\t' => 't'
            case _    => (if (Character.isISOControl(c)) 1 else 0).toChar
          }
          if (esc =!= 0) {
            builder.append(value, offset, i).append('\\')
            if (esc =!= 1) builder.append(esc)
            else
              builder
                .append('u')
                .append(toHex((c >> 12) & 15))
                .append(toHex((c >> 8) & 15))
                .append(toHex((c >> 4) & 15))
                .append(toHex(c & 15))
            offset = i + 1
          }

          i += 1
        }

        if (offset < i) builder.append(value, offset, i)
        builder.append('"')
        state
      }
    }

  private final case class FormatterState
    (builder: StringBuilder, currentIndent: Int, maxWidth: MaxWidth, isMultiLine: Boolean) {
    def append(str: String): FormatterState = {
      builder.appendAll(str)
      this
    }

    def appendN(count: Int, c: Char): FormatterState = {
      builder.appendAll(Array.fill(count)(c))
      this
    }

    def concat(other: FormatterState): FormatterState = {
      builder.appendAll(other.builder)
      this
    }

    def indentAndRecurse(indentDelta: Delta): FormatterState = FormatterState(
      builder = new StringBuilder(),
      currentIndent = currentIndent + Delta.raw(indentDelta),
      maxWidth = maxWidth,
      isMultiLine = false
    )
  }

  private object FormatterState {
    def empty(maxWidth: MaxWidth): FormatterState = FormatterState(new StringBuilder(), 0, maxWidth, isMultiLine = false)
  }
}
