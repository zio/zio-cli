---
id: figlet
title: "Figlet banners"
---
A `CliApp` prints a banner made of ASCII art. The banner shows the application name in the help output and in the header of wizard mode. The art is produced from a FIGlet font. FIGlet is a text format for fonts made of ASCII characters. A font file has the `.flf` extension and contains one drawing for each character.

zio-cli includes one font, `FigFont.Default`, so a banner works without any setup. You can also load another font, on the JVM only, from a file, a resource, or a URL.

## Default banner

`CliApp.make` has a `figFont` parameter. If you do not pass it, it uses `FigFont.Default`.

```scala mdoc:silent:reset
import zio._
import zio.cli._

val greetCommand = Command("hello", Options.text("name"))

val greetApp = CliApp.make(
  name = "hello",
  version = "1.0.0",
  summary = HelpDoc.Span.text("Greets someone."),
  command = greetCommand
)(name => Console.printLine(s"Hello, $name!"))
```

`FigFont.Default` uses the "Slant" font. It is stored as a string in `zio/cli/figlet/FigFontFiles.scala` and is parsed the first time it is used.

## Choosing a font

Pass the font to `CliApp.make` with the `figFont` parameter:

```scala mdoc:silent
import zio.cli.figlet.FigFont

val explicitApp = CliApp.make(
  name = "hello",
  version = "1.0.0",
  summary = HelpDoc.Span.text("Greets someone."),
  command = greetCommand,
  figFont = FigFont.Default
)(name => Console.printLine(s"Hello, $name!"))
```

You can see the rendered text without running the app:

```scala mdoc:silent
val bannerText: String = FigFont.Default.render("hello")
```

`render` returns a single `String` with line breaks. `renderLines` returns the same lines as a `Chunk[String]`.

## Loading a font

`FigFont` is a case class. `FigFont.fromLines` parses a font from lines of text. It is available on every platform:

```scala
FigFont.fromLines(lines: Iterable[String]): Either[String, FigFont]
```

The loaders below read a font from a source. They are available only on the JVM. Each one returns a `ZIO` value that fails with `Either[IOException, String]`:

- `Left(ioException)` means the file, resource, or URL could not be read.
- `Right(message)` means the text was read but is not a valid font. The message starts with the line number.

### Loading from a file

```scala mdoc:silent
import java.io.IOException

val fontFromFile: ZIO[Any, Either[IOException, String], FigFont] =
  FigFont.fromFile("fonts/small.flf")
```

`fromFile` reads the file with ISO-8859-1 encoding.

### Loading from a classpath resource

```scala mdoc:silent
val fontFromResource: ZIO[Any, Either[IOException, String], FigFont] =
  FigFont.fromResource("fonts/small.flf", getClass.getClassLoader)
```

### Loading from a URL

This example makes a network request when it runs, so it is compiled but not run during the docs build.

```scala mdoc:compile-only
import java.io.IOException

val fontFromUrl: ZIO[Any, Either[IOException, String], FigFont] =
  FigFont.fromURL("https://example.com/fonts/small.flf")
```

### Loading from any source

`fromSource` takes a `ZIO` that produces a `scala.io.Source`. The other loaders are built on top of it. Use it when your font comes from somewhere else, such as a database. The source is closed after the lines are read.

## Using a loaded font

The following program loads a font file when it starts. If the file cannot be loaded, it falls back to `FigFont.Default`.

```scala mdoc:compile-only
import zio._
import zio.cli._
import zio.cli.figlet.FigFont

object BannerApp extends ZIOAppDefault {
  private val command = Command("hello", Options.text("name"))

  private def app(font: FigFont) =
    CliApp.make(
      name = "hello",
      version = "1.0.0",
      summary = HelpDoc.Span.text("Greets someone."),
      command = command,
      figFont = font
    )(name => Console.printLine(s"Hello, $name!"))

  def run =
    for {
      args <- ZIOAppArgs.getArgs
      font <- FigFont.fromFile("fonts/small.flf").orElseSucceed(FigFont.Default)
      _    <- app(font).run(args.toList)
    } yield ()
}
```

## Platform support

The core parser and renderer are shared code and work on the JVM, JS, and Native. Only the loaders that read from a source are JVM-only.

| Method | JVM | JS | Native |
| --- | --- | --- | --- |
| `FigFont.Default` | Yes | Yes | Yes |
| `FigFont.fromLines` | Yes | Yes | Yes |
| `FigFont.fromFile` | Yes | No | No |
| `FigFont.fromResource` | Yes | No | No |
| `FigFont.fromURL` | Yes | No | No |
| `FigFont.fromSource` | Yes | No | No |

On JS and Native, a program that calls a JVM-only loader does not compile. To load a font there, read the file yourself and pass its lines to `FigFont.fromLines`.

## Internal code

The following are internal to zio-cli. Do not use them in your code:

- `FigFontParser` and `FigFontRenderer` are `private[figlet]`.
- `Parser` in the `zio.cli.figlet` package is a set of parser combinators used by `FigFontParser`.

## See also

- [Built-in commands](built-in-commands.md) for `--help` and the wizard, which show the banner.
- [Commands](commands.md) for how a `Command` is built.
