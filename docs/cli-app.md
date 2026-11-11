---
id: cli-app
title: "CliApp and running a CLI"
---

A `CliApp` is a complete description of a command-line application. It holds the program's name, version and summary, the `Command` that describes what the user may type, and the function that runs once the input is parsed. A `CliApp[R, E, A]` needs an environment `R`, can fail with an error of type `E`, and produces a value of type `A`.

You build a `CliApp` with `CliApp.make`, run it with `CliApp.run`, and, for a stand-alone program, wrap it in `ZIOCliDefault`. The sections below cover each step, then the usage line that the help output prints.

## Building a CliApp with CliApp.make

`CliApp.make` takes the description as its first parameter list and the function that runs the command as its second.

```scala mdoc:silent
import zio._
import zio.cli._
import zio.cli.HelpDoc.Span.text

val nameOpt: Options[String] = Options.text("name").withDefault("world") ?? "Who to greet."
val greet: Command[String]   = Command("greet", nameOpt)

val greetApp = CliApp.make(
  name = "greet",
  version = "1.0.0",
  summary = text("Prints a greeting"),
  command = greet,
  footer = HelpDoc.p("Example footer text.")
)(name => Console.printLine(s"Hello, $name!"))
```

Each parameter of `CliApp.make` has the following meaning.

| Parameter | Type | Default | What it does |
|---|---|---|---|
| `name` | `String` | none | The program name. It is shown in the help header and used for the banner. If the command has no names of its own, the shell completion script uses it. |
| `version` | `String` | none | The version string. It is shown in the help header after the name. |
| `summary` | `HelpDoc.Span` | none | A one-line description. It is shown in the help header after `--`. |
| `command` | `Command[Model]` | none | The grammar of valid input. The value it parses is the `Model` passed to `execute`. See [Commands](commands.md). |
| `footer` | `HelpDoc` | `HelpDoc.Empty` | Text printed after the options and arguments in the help output. |
| `config` | `CliConfig` | `CliConfig.default` | Parsing and help settings, such as case sensitivity and autocorrection. See [CLI configuration](cli-config.md). |
| `figFont` | `FigFont` | `FigFont.Default` | The font used to draw the name as a banner in the help output. See [Figlet banners](figlet.md). |
| `execute` | `Model => ZIO[R, E, A]` | none | The second parameter list. It runs with the parsed value of the command. |

The `footer`, `config` and `figFont` parameters can be left out. The other parameters are required. The `config` parameter can also be changed later with `CliApp.config`.

## Running a CliApp with CliApp.run

`run` takes the raw arguments as a `List[String]` and returns `ZIO[R, CliError[E], Option[A]]`. The arguments do not include the program name.

```scala mdoc:compile-only
import zio._
import zio.cli._

object GreetMain extends ZIOAppDefault {
  def run =
    greetApp.run(List("--name", "Ada"))
}
```

`run` behaves as follows.

1. It parses the arguments against the command. If the command is a single command, its name is added in front of the arguments for you. If the command has subcommands, you type the subcommand name yourself.
2. If the input is valid and is a user command, it runs `execute` and returns `Some(result)`.
3. If the input asks for a built-in action, such as `--help`, it prints the output and returns `None`. See [Built-in commands](built-in-commands.md).
4. If the input is invalid, it prints the error message and fails with `CliError.Parsing`.

The error type `CliError[E]` has four cases:

- `CliError.Parsing(e: ValidationError)`: the input did not match the command. The message is already printed when this case is returned.
- `CliError.BuiltIn(e: Exception)`: a built-in action failed, for example the wizard.
- `CliError.IO(e: IOException)`: printing a result or a built-in output failed.
- `CliError.Execution[E](e: E)`: `execute` failed with your error type `E`.

For the meaning of `ValidationError` and how to handle these cases, see [Validation and errors](validation-and-errors.md).

## Running a stand-alone program with ZIOCli and ZIOCliDefault

`ZIOCli` is a trait that extends `ZIOApp`. You implement `cliApp`, and the trait reads the program arguments and calls `cliApp.run`. If the input is invalid, the trait returns the failure exit code (`ExitCode.failure`, which is 1). Other errors are passed on as ZIO failures.

`ZIOCliDefault` is `ZIOCli` combined with `ZIOAppDefault`. It is the usual way to write a command-line program.

```scala mdoc:compile-only
import zio.cli._

object Greet extends ZIOCliDefault {
  val cliApp = greetApp
}
```

Use `ZIOCli` directly when your program already extends another `ZIOApp` subtype. Use `ZIOCliDefault` otherwise.

## Changing a CliApp

Each combinator returns a new `CliApp` and leaves the original unchanged.

### map

`map` changes the value that `execute` returns. Built-in actions such as `--help` return `None`, so `map` does not run for them.

```scala mdoc:silent
val greetingApp = CliApp.make(
  name = "greet",
  version = "1.0.0",
  summary = text("Builds a greeting"),
  command = greet
)(name => ZIO.succeed(s"Hello, $name!"))

val lengthApp = greetingApp.map(_.length)
```

### flatMap

`flatMap` runs another effect after `execute` finishes, with the value that `execute` returned. The environment and error types of the new `CliApp` are widened to include those of the new effect.

```scala mdoc:silent
val loggedApp = greetApp.flatMap(_ => Console.printLine("Greeting sent."))
```

### footer

`footer` appends a `HelpDoc` to the footer. Calling it more than once keeps all the text, in call order.

```scala mdoc:silent
val withFooter = greetApp.footer(HelpDoc.p("Report problems to the issue tracker."))
```

### summary

`summary` appends a `HelpDoc.Span` to the summary in the header line. The summary given to `make` stays in place, and the new span is added after it.

```scala mdoc:silent
val withSummary = greetApp.summary(text(" (beta)"))
```

## The usage line

The help output has a `USAGE` section. Each line of that section is one shape the command accepts. The shape comes from a `UsageSynopsis` value, which every `Command`, `Options` and `Args` has.

The synopsis is built as follows.

1. A command starts with its name. For example, `Command("greet", ...)` starts with `greet`.
2. The synopsis of the options is added after the name, then the synopsis of the arguments.
3. Each option is written as its name and, unless it is a flag, its type. A text option is `--name <text>`. An enumeration option shows its choices, such as `a | b`.
4. Options with a default value, and options made with `.optional`, are wrapped in square brackets. A flag such as `Options.boolean("verbose")` is therefore `[--verbose]`.
5. Two options combined with `|` are shown with a `|` between them and no spaces, such as `--a|--b`. When the options are optional, each one is in its own brackets, such as `[--a]|[--b]`.
6. A repeated argument, such as one written with `*` or `+`, is followed by `...`.
7. A command that has subcommands shows `<command>` in place of the subcommands.
8. Empty parts are removed, so a command with no options and no arguments shows only its name.

For the `greet` command above, the USAGE section is:

```
USAGE

  $ greet [--name <text>]
```

Each line starts with `$ `. This is the only output the synopsis produces. Two settings in `CliConfig` change how names and types appear in the line: `showAllNames` and `showTypes`. They are described in [CLI configuration](cli-config.md#help-appearance).

You can read the synopsis in code with `Command.synopsis`, `Options.synopsis` and `Args.synopsis`. Each returns a `UsageSynopsis`. Its `enumerate(config)` method returns the lines as a `List[HelpDoc.Span]`, and its `helpDoc` method returns the same lines as a `HelpDoc`.

```scala mdoc:silent
val synopsisLines = greet.synopsis.enumerate(CliConfig.default)
```

Use the public methods above. The case classes that make up a synopsis, such as `UsageSynopsis.Named` and `UsageSynopsis.Optional`, are public, but you rarely need to build them yourself. They are created by the constructors of `Options` and `Args`.
