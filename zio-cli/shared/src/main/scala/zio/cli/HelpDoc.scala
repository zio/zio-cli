package zio.cli

/**
 * A `HelpDoc` is a help document made of headers, paragraphs, description lists and enumerations, put one after the other with `+`. The text inside them is made of [[HelpDoc.Span]]s. ZIO CLI builds the help of commands, options and arguments as `HelpDoc`s and prints them with `toPlaintext`; `toHTML` renders an HTML page instead.
 *
 * {{{
 * val doc: HelpDoc =
 *   HelpDoc.h1("Examples") +
 *     HelpDoc.p("Copy a file:") +
 *     HelpDoc.p(HelpDoc.Span.code("cp a.txt b.txt"))
 *
 * val text: String = doc.toPlaintext(columnWidth = 80, color = false)
 * }}}
 *
 * @see [[https://zio.dev/zio-cli/helpdoc/ HelpDoc]]
 */
sealed trait HelpDoc { self =>
  import HelpDoc._

  /**
   * Puts `that` after this document. When either side is empty (see `isEmpty`), returns the other side.
   */
  def +(that: HelpDoc): HelpDoc =
    (self, that) match {
      case (self, that) if self.isEmpty => that
      case (self, that) if that.isEmpty => self
      case _                            => HelpDoc.Sequence(self, that)
    }

  /**
   * Returns this document, or `that` when this one is empty (see `isEmpty`).
   */
  def |(that: HelpDoc): HelpDoc = if (self.isEmpty) that else self

  /**
   * Returns the text of a header or a paragraph. Any other document, including a sequence that starts with a paragraph, gives an empty span.
   */
  def getSpan: Span =
    self match {
      case HelpDoc.Header(value, _) => value
      case HelpDoc.Paragraph(value) => value
      case _                        => HelpDoc.Span.empty
    }

  /**
   * Returns `true` for `HelpDoc.Empty`, for a description list whose descriptions are all empty (whatever its terms), for an enumeration whose elements are all empty, and for a sequence of two empty documents. A header or a paragraph returns `false`, even with empty text.
   */
  def isEmpty: Boolean =
    self match {
      case HelpDoc.Empty                 => true
      case HelpDoc.DescriptionList(xs)   => xs.forall(_._2.isEmpty)
      case HelpDoc.Sequence(left, right) => left.isEmpty && right.isEmpty
      case HelpDoc.Enumeration(xs)       => xs.forall(_.isEmpty)
      case _                             => false
    }

  /**
   * Returns `true` for a header, or for a sequence whose first part is a header.
   */
  def isHeader: Boolean =
    self match {
      case HelpDoc.Header(_, _)      => true
      case HelpDoc.Sequence(left, _) => left.isHeader
      case _                         => false
    }

  /**
   * Returns `true` for a paragraph, or for a sequence whose first part is a paragraph.
   */
  def isParagraph: Boolean =
    self match {
      case HelpDoc.Paragraph(_)      => true
      case HelpDoc.Sequence(left, _) => left.isParagraph
      case _                         => false
    }

  /**
   * Returns `true` for a description list, or for a sequence whose first part is a description list.
   */
  def isDescriptionList: Boolean =
    self match {
      case HelpDoc.DescriptionList(_) => true
      case HelpDoc.Sequence(left, _)  => left.isDescriptionList
      case _                          => false
    }

  /**
   * Returns `true` for an enumeration, or for a sequence whose first part is an enumeration.
   */
  def isEnumeration: Boolean =
    self match {
      case HelpDoc.Enumeration(_)    => true
      case HelpDoc.Sequence(left, _) => left.isEnumeration
      case _                         => false
    }

  /**
   * Returns `true` for a sequence built by `+`.
   */
  def isSequence: Boolean =
    self match {
      case HelpDoc.Sequence(_, _) => true
      case _                      => false
    }

  /**
   * Applies `f` to each (term, description) entry of a description list. Returns any other document unchanged, including a sequence that contains description lists.
   */
  def mapDescriptionList(f: (HelpDoc.Span, HelpDoc) => (HelpDoc.Span, HelpDoc)): HelpDoc =
    self match {
      case HelpDoc.DescriptionList(list) => HelpDoc.DescriptionList(list.map(f.tupled))
      case x                             => x
    }

  /**
   * Renders this document as a complete HTML page with a built-in style sheet. Headers, paragraphs and enumerations become the matching HTML elements, code spans become preformatted blocks, and the parts of a sequence are separated by line breaks. The entries of a description list are written in reverse order, each term followed by its description.
   */
  def toHTML: String = {

    val w = new StringBuilder

    val escape: String => String =
      _.replaceAll("<", "&lt;")
        .replaceAll(">", "&gt;")

    def renderSpan: Span => StringBuilder = {
      case Span.Text(value) => w.append(escape(value))
      case Span.Code(value) => w.append(s"<pre><code>${escape(value)}</code></pre>")
      case Span.URI(value)  => w.append(s"""<a href="$value">$value</a>""")
      case Span.Weak(value) => renderSpan(value)

      case Span.Strong(value) =>
        w.append("<b>")
        renderSpan(value)
        w.append("</b>")

      case Span.Error(value) =>
        w.append(s"<span class='error'>")
        renderSpan(value)
        w.append("</span>")

      case Span.Sequence(left, right) =>
        renderSpan(left)
        renderSpan(right)
    }

    def render: HelpDoc => StringBuilder = {
      case HelpDoc.Empty => w

      case HelpDoc.Header(value, level) =>
        w.append(s"<h$level>")
        renderSpan(value)
        w.append(s"</h$level>")

      case HelpDoc.Paragraph(value) =>
        w.append(s"<p>")
        renderSpan(value)
        w.append(s"</p>")

      case HelpDoc.DescriptionList(definitions) =>
        definitions.foldRight(w) { case ((span, helpDoc), _) =>
          renderSpan(span)
          render(helpDoc)
        }

      case HelpDoc.Enumeration(elements) =>
        w.append("<ul>")
        elements.foreach { hd =>
          w.append("<li>")
          render(hd)
          w.append("</li>")
        }
        w.append("</ul>")

      case HelpDoc.Sequence(left, right) =>
        render(left)
        w.append("<br/>")
        render(right)
    }

    val css =
      """<style>
        h1 {
            color: rgb(36, 41, 46);
            font-weight: 600;
            line-height: 1.25;
            margin-bottom: 16px;
            font-family: -apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif, Apple Color Emoji, Segoe UI Emoji, Segoe UI Symbol;
        }

        h2 {
            font-family: -apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif, Apple Color Emoji, Segoe UI Emoji, Segoe UI Symbol;
            font-size: 24px;
            letter-spacing: 0px;
            word-spacing: 2px;
            color: rgb(36, 41, 46);
            font-weight: 600;
        }

        h3 {
            font-family: -apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif, Apple Color Emoji, Segoe UI Emoji, Segoe UI Symbol;
            font-size: 21px;
            letter-spacing: 0px;
            word-spacing: 2px;
            color: rgb(36, 41, 46);
            font-weight: 700;
        }

        p {
            font-family: -apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif, Apple Color Emoji, Segoe UI Emoji, Segoe UI Symbol;
            color: #24292e;
        }

        .error {
            font-family: -apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif, Apple Color Emoji, Segoe UI Emoji, Segoe UI Symbol;
            color: #24292e;
        }

        a {
            border: 0;
            color: rgb(189, 39, 26);
            text-decoration: none;
            font-family: -apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif, Apple Color Emoji, Segoe UI Emoji, Segoe UI Symbol;
            font-size: inherit;
            font-size: 100%;
            margin: 0;
            padding: 0;
            vertical-align: baseline;
        }

        a:hover {
            color: rgb(0, 0, 0);
        }

        pre {
          background-color: rgba(27, 31, 35, .05);
        }

        code {
            border-radius: 3px;
            color: rgb(36, 41, 46);
            font-family: SFMono-Regular, Menlo, Monaco, Consolas, Liberation Mono, Courier New, monospace;
            font-size: 85%;
            margin: 0;
        }
    </style>"""

    w.append(s"<html><head>$css</head><body>")
    render(this)
    w.append("</body></html>")

    w.toString()
  }

  /**
   * Renders this document as text for a terminal, wrapping lines at `columnWidth` characters. Headers are written in upper case whatever their level, each description in a description list goes under its term, indented by two spaces, and each element of an enumeration starts with `- `.
   *
   * With `color` (the default), ANSI color codes make headers and terms bold, errors red and URIs underlined; with `color = false`, the text has no such codes.
   *
   * {{{
   * val text = (HelpDoc.h1("usage") + HelpDoc.p("Run the tool.")).toPlaintext(color = false)
   * // "\nUSAGE\n\n  Run the tool.\n"
   * }}}
   */
  def toPlaintext(columnWidth: Int = 100, color: Boolean = true): String = {
    val _ = color

    val writer     = DocWriter(0, columnWidth)
    var uppercase  = false
    var styles     = List.empty[String]
    var lastStyle  = Console.RESET
    var printedSep = 0

    def setStyle(style: String): Unit = styles = style :: styles

    def currentStyle(): String = styles.headOption.getOrElse(Console.RESET)

    def resetStyle(): Unit = styles = styles.drop(1)

    def renderText(text: String): Unit =
      renderSpan(Span.text(text))

    def renderNewline(): Unit =
      if (printedSep < 2) {
        printedSep += 1
        val _ = writer.append("\n")
      }

    def clearSep() = printedSep = 0

    def renderHelpDoc(helpDoc: HelpDoc): Unit =
      helpDoc match {
        case Empty                    =>
        case HelpDoc.Header(value, _) =>
          writer.unindent()
          renderNewline()
          uppercase = true
          setStyle(Console.BOLD)
          renderSpan(value)
          resetStyle()
          uppercase = false
          renderNewline()
          writer.indent(2)

        case HelpDoc.Paragraph(value) =>
          renderSpan(value)
          renderNewline()

        case HelpDoc.DescriptionList(definitions) =>
          definitions.zipWithIndex.foreach { case ((span, helpDoc), _) =>
            setStyle(Console.BOLD)
            renderSpan(span)
            resetStyle()
            renderNewline()
            writer.indent(2)
            renderHelpDoc(helpDoc)
            writer.unindent()
            renderNewline()
          }

        case HelpDoc.Enumeration(elements) =>
          elements.zipWithIndex.foreach { case (helpDoc, _) =>
            renderText("- ")
            renderHelpDoc(helpDoc)
          }
          writer.unindent()

        case HelpDoc.Sequence(left, right) =>
          renderHelpDoc(left)
          renderNewline()
          renderHelpDoc(right)
      }

    def renderSpan(span: Span): Unit = {
      clearSep()
      val _ = span match {
        case Span.Text(value) =>
          if (color && (lastStyle != currentStyle())) {
            writer.append(currentStyle())
            lastStyle = currentStyle()
          }

          writer.append(if (uppercase) value.toUpperCase() else value)

        case Span.Code(value) =>
          setStyle(Console.WHITE)
          writer.append(value)
          resetStyle()

        case Span.Error(value) =>
          setStyle(Console.RED)
          renderSpan(value)
          resetStyle()

        case Span.Weak(value) =>
          setStyle(Console.BOLD)
          renderSpan(value)
          resetStyle()

        case Span.Strong(value) =>
          setStyle(Console.BOLD)
          renderSpan(value)
          resetStyle()

        case Span.URI(value) =>
          setStyle(Console.UNDERLINED)
          renderSpan(Span.text(value.toASCIIString()))
          resetStyle()

        case Span.Sequence(left, right) =>
          renderSpan(left)
          renderSpan(right)
      }
    }

    renderHelpDoc(this)

    writer.toString() + (if (color) Console.RESET else "")
  }
}
/**
 * The cases of [[HelpDoc]] and the constructors for documents and spans.
 */
object HelpDoc {
  /**
   * The empty document. `+` drops it.
   */
  case object Empty                                                    extends HelpDoc
  /**
   * A header with the text `value`. `h1`, `h2` and `h3` set `level` to 1, 2 or 3; `toHTML` uses the level, while `toPlaintext` writes every header the same way.
   */
  final case class Header(value: Span, level: Int)                     extends HelpDoc
  /**
   * A paragraph with the text `value`.
   */
  final case class Paragraph(value: Span)                              extends HelpDoc
  /**
   * A list of terms, each followed by its description, such as the options of a command and what they do.
   */
  final case class DescriptionList(definitions: List[(Span, HelpDoc)]) extends HelpDoc
  /**
   * A list of documents shown as bullet points.
   */
  final case class Enumeration(elements: List[HelpDoc])                extends HelpDoc { self =>
    /**
     * Moves the elements of each enumeration directly inside this one into it. Only one level is flattened.
     */
    def flatten: Enumeration =
      Enumeration(
        self.elements.flatMap {
          case Enumeration(elements) => elements
          case other                 => List(other)
        }
      )
  }

  /**
   * Two documents, one after the other; `+` builds it.
   */
  final case class Sequence(left: HelpDoc, right: HelpDoc) extends HelpDoc

  /**
   * Puts the documents of `bs` one after the other, dropping empty ones; an empty collection gives `HelpDoc.Empty`.
   */
  def blocks(bs: Iterable[HelpDoc]): HelpDoc =
    if (bs.isEmpty) HelpDoc.Empty else blocks(bs.head, bs.tail.toSeq: _*)

  /**
   * Puts `helpDoc` and the documents of `helpDocs0` one after the other, dropping empty ones.
   */
  def blocks(helpDoc: HelpDoc, helpDocs0: HelpDoc*): HelpDoc =
    helpDocs0.foldLeft(helpDoc)(_ + _)

  /**
   * Creates a description list from (term, description) pairs.
   */
  def descriptionList(definitions: (Span, HelpDoc)*): HelpDoc = HelpDoc.DescriptionList(definitions.toList)

  /**
   * The empty document, `HelpDoc.Empty`.
   */
  val empty: HelpDoc = Empty

  /**
   * Creates an enumeration of `elements`. An element that is itself an enumeration has its elements moved into this one.
   */
  def enumeration(elements: HelpDoc*): HelpDoc =
    HelpDoc.Enumeration(elements.toList).flatten

  /**
   * Creates a level-1 header with the text `t`.
   */
  def h1(t: String): HelpDoc  = h1(Span.text(t))
  /**
   * Creates a level-1 header with the text `span`.
   */
  def h1(span: Span): HelpDoc = HelpDoc.Header(span, 1)

  /**
   * Creates a level-2 header with the text `t`.
   */
  def h2(t: String): HelpDoc  = h2(Span.text(t))
  /**
   * Creates a level-2 header with the text `span`.
   */
  def h2(span: Span): HelpDoc = HelpDoc.Header(span, 2)

  /**
   * Creates a level-3 header with the text `t`.
   */
  def h3(t: String): HelpDoc  = h3(Span.text(t))
  /**
   * Creates a level-3 header with the text `span`.
   */
  def h3(span: Span): HelpDoc = HelpDoc.Header(span, 3)

  /**
   * Creates a paragraph with the text `t`.
   */
  def p(t: String): HelpDoc  = HelpDoc.Paragraph(Span.text(t))
  /**
   * Creates a paragraph with the text `span`.
   */
  def p(span: Span): HelpDoc = HelpDoc.Paragraph(span)

  /**
   * A piece of inline text in a [[HelpDoc]]: plain text, code, an error, weak or strong text, a URI, or two spans one after the other.
   */
  sealed trait Span { self =>
    /**
     * Puts `that` after this span.
     */
    final def +(that: Span): Span = Span.Sequence(self, that)

    /**
     * Returns `true` when `size` is 0.
     */
    final def isEmpty: Boolean = self.size == 0

    /**
     * Returns the number of characters of the text, counting a URI by its string form.
     */
    final def size: Int =
      self match {
        case Span.Text(value)           => value.length
        case Span.Code(value)           => value.length
        case Span.Error(value)          => value.size
        case Span.Weak(value)           => value.size
        case Span.Strong(value)         => value.size
        case Span.URI(value)            => value.toString.length
        case Span.Sequence(left, right) => left.size + right.size
      }

    /**
     * Returns the text without any styling; a URI gives its string form.
     */
    final def text: String =
      self match {
        case Span.Text(value)           => value
        case Span.Code(value)           => value
        case Span.Error(value)          => value.text
        case Span.Weak(value)           => value.text
        case Span.Strong(value)         => value.text
        case Span.URI(value)            => value.toString
        case Span.Sequence(left, right) => left.text + right.text
      }
  }
  /**
   * The cases of [[HelpDoc.Span]] and their constructors.
   */
  object Span {
    /**
     * Plain text.
     */
    final case class Text(value: String)               extends Span
    /**
     * Code, such as a command line to type. `toHTML` writes it as a preformatted block.
     */
    final case class Code(value: String)               extends Span
    /**
     * A span marked as an error, as in the messages of parse errors. `toPlaintext` writes it in red when colors are on.
     */
    final case class Error(value: Span)                extends Span
    /**
     * A span with weak emphasis, used for the names of arguments in help.
     */
    final case class Weak(value: Span)                 extends Span
    /**
     * A span with strong emphasis. `toPlaintext` writes it in bold when colors are on, and `toHTML` in bold.
     */
    final case class Strong(value: Span)               extends Span
    /**
     * A link to `value`. `toPlaintext` writes it underlined when colors are on, and `toHTML` as a link.
     */
    final case class URI(value: java.net.URI)          extends Span
    /**
     * Two spans, one after the other; `+` builds it.
     */
    final case class Sequence(left: Span, right: Span) extends Span

    /**
     * Creates a code span with the text `t`.
     */
    def code(t: String): Span = Span.Code(t)

    /**
     * A span with no text, `Text("")`.
     */
    def empty: Span = Span.text("")

    /**
     * Marks `span` as an error.
     */
    def error(span: Span): Span = Span.Error(span)

    /**
     * Creates an error span with the text `t`.
     */
    def error(t: String): Span = Span.Error(text(t))

    /**
     * A span with a single space.
     */
    def space: Span = text(" ")

    /**
     * Puts the spans one after the other, after an empty text span.
     */
    def spans(span: Span, spans0: Span*): Span = spans(span :: spans0.toList)

    /**
     * Puts the spans of `spans` one after the other, after an empty text span; an empty collection gives an empty text span.
     */
    def spans(spans: Iterable[Span]): Span =
      spans.toList.foldLeft(text("")) { case (span, s) =>
        Span.Sequence(span, s)
      }

    /**
     * Gives `span` strong emphasis.
     */
    def strong(span: Span): Span = Span.Strong(span)

    /**
     * Creates a strong span with the text `t`.
     */
    def strong(t: String): Span = Span.Strong(text(t))

    /**
     * Creates a plain text span.
     */
    def text(t: String): Span = Span.Text(t)

    /**
     * Creates a link to `uri`.
     */
    def uri(uri: java.net.URI): Span = Span.URI(uri)

    /**
     * Gives `span` weak emphasis.
     */
    def weak(span: Span): Span = Span.Weak(span)

    /**
     * Creates a weak span with the text `t`.
     */
    def weak(t: String): Span = Span.Weak(text(t))
  }
}

private[cli] class DocWriter(stringBuilder: StringBuilder, startOffset: Int, columnWidth: Int) { self =>
  private var marginStack: List[Int] = List(self.startOffset)

  def append(s: String): DocWriter = {
    if (s.isEmpty) self
    else
      DocWriter.splitNewlines(s) match {
        case None =>
          if (self.currentColumn + s.length > self.columnWidth) {
            val remainder = self.columnWidth - self.currentColumn

            val lastSpace = {
              val lastSpace = s.take(remainder + 1).lastIndexOf(' ')

              if (lastSpace == -1) remainder else lastSpace
            }

            val before = s.take(lastSpace)
            val after  = s.drop(lastSpace).dropWhile(_ == ' ')

            append(before)
            append("\n")
            append(after)
          } else {
            val padding = self.currentMargin - self.currentColumn
            if (padding > 0) {
              self.stringBuilder.append(DocWriter.margin(padding))
              self.currentColumn += padding
            }
            self.stringBuilder.append(s)
            self.currentColumn += s.length
          }
        case Some(pieces) =>
          pieces.zipWithIndex.foreach { case (piece, _) =>
            append(piece)

            self.stringBuilder.append("\n")
            self.currentColumn = 0
          }
      }

    this
  }

  def currentMargin: Int = self.marginStack.sum

  var currentColumn: Int = self.startOffset

  def indent(adjust: Int): Unit = self.marginStack = adjust :: self.marginStack

  override def toString(): String = stringBuilder.toString()

  def unindent(): Unit = self.marginStack = self.marginStack.drop(1)
}
private[cli] object DocWriter {
  private def margin(n: Int): String                  = if (n <= 0) "" else List.fill(n)(" ").mkString
  def splitNewlines(s: String): Option[Array[String]] = {
    val count = s.count(_ == '\n')

    if (count == 0) None
    else
      Some {
        val size = if (count == s.length) count else count + 1

        val array = Array.ofDim[String](size)

        var i = 0
        var j = 0
        while (i < s.length) {
          val search   = s.indexOf('\n', i)
          val endIndex = if (search == -1) s.length else search

          array(j) = s.substring(i, endIndex)

          i = endIndex + 1
          j = j + 1
        }
        if (j < array.length) array(j) = ""

        array
      }
  }

  def apply(startOffset: Int, columnWidth: Int): DocWriter = {
    val builder = new StringBuilder
    builder.append(margin(startOffset))
    new DocWriter(builder, startOffset, if (columnWidth <= 0) startOffset + 1 else columnWidth)
  }
}
