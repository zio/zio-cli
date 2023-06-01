---
id: helpdoc
title: "Help Documentation"
sidebar_label: "Help Documentation"
---
`HelpDoc` is a description of the documentation of a CLI App. They can be added to any `Command`, `Options` or `Args`.
`HelpDoc` is composed of a list of `HelpDoc` items that can be headers, paragraphs, description lists, sequences and enumerations.

## Building blocks

```scala mdoc:silent
import zio.cli._
import zio.cli.HelpDoc.Span

val t = "text"

val element: HelpDoc  = HelpDoc.empty           // HelpDoc without content
val element1: HelpDoc  = HelpDoc.h1(t: String)  // Header of level 1
val element2: HelpDoc  = HelpDoc.h2(t: String)  // Header of level 2
HelpDoc.h3(t: String): HelpDoc                  // Header of level 3
HelpDoc.p(t: String): HelpDoc                   // Paragraph
  
// Enumeration of HelpDocs
HelpDoc.enumeration(element: HelpDoc): HelpDoc
HelpDoc.enumeration(element1: HelpDoc, element2: HelpDoc)
  
// Creates a list with Span as header
val definition1 = (Span.text("span1"), element1)
val definition2 = (Span.text("span2"), element2)
HelpDoc.descriptionList(definition1: (Span, HelpDoc)): HelpDoc
HelpDoc.descriptionList(definition1: (Span, HelpDoc), definition2: (Span, HelpDoc))
  
// Stacks HelpDocs
HelpDoc.blocks(element: HelpDoc): HelpDoc
HelpDoc.blocks(element1: HelpDoc, element2: HelpDoc)

```

### Span
The data of a HelpDoc is not stored as text, rather as `Span` type, that also contains information about the type of information. It is possible to use methods `h1`, `h2`, `h3` and `p` with `Span` instead of `String`. You can create `Span` instances using the following methods:
```scala mdoc:silent

val span = Span.code(t: String)
Span.empty
Span.error(span: Span)
Span.error(t: String)
Span.space
Span.spans(span: Span)                // You can add more than one span
Span.spans(span, span)
Span.spans(List(span))
Span.strong(span: Span)
Span.strong(t: String)
Span.text(t: String)
Span.uri(java.net.URI.create("https://zio.dev/"))
Span.weak(span: Span)
Span.weak(t: String)
```
You can also obtain its text value using `span.text` or concatenate `Span` using `span1 + span2`.

## Transformation methods
A `HelpDoc` can be converted into plaintext and HTML:
```scala mdoc:silent:reset
trait HelpDoc {
  def toHTML: String
  def toPlaintext(columnWidth: Int = 100, color: Boolean = true): String
}
```

## Combination methods
```scala mdoc:silent:reset
trait HelpDoc {
  def +(that: HelpDoc): HelpDoc     // Concatenate HelpDocs in successive levels
  def |(that: HelpDoc): HelpDoc
}
```
To combine HelpDocs, we can use `HelpDoc.DescriptionList`, `HelpDoc.Enumeration`, and `HelpDoc.Sequence`.

## Examples
The more common use case is through the operators `??` and `withHelp`.
- `??` can be applied to `Options` and `Args`. It adds a string to the current description.
```scala mdoc:silent:reset
import zio.cli._

trait Options[A] {
  def ??(that: String): Options[A] // or Args[A]
}

val optionsWithHelp = Options.text("sample") ?? "description of options"
```

- `withHelp` is applied to `Command`. It overwrites the current help of the command, so use it cautiously! On the other hand, you need to use it to add your desired `HelpDoc`
```scala mdoc:silent:reset
import zio.cli._

trait Options[A] {
  def withHelp(that: String): Command[A] // that is converted into a paragraph
  def withHelp(that: HelpDoc): Command[A]
}

val optionsWithHelp = Options.text("sample") ?? "description of options"
val commandWithHelp = Command("command", optionsWithHelp).withHelp("description of command")
```
When `withHelp` is used with a command that has parent and children subcommands, it is applied only to the parent command.

If a more complex use is desired, it is possible to combine headers and paragraphs using the methods before.
```scala mdoc:silent
// Create a HelpDoc with a header depending on an integer
val paragraph = HelpDoc.p("paragraph")
def header(n: Int) = HelpDoc.h1("Header: " + n)
def completeDoc(n: Int) = header(n) + paragraph

// Create an enumeration of HelpDocs
val complexDoc = HelpDoc.enumeration(completeDoc(7), completeDoc(-1), completeDoc(3))
```


