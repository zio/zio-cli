---
id: validation-and-errors
title: "Validation and errors"
---
This page explains the errors that a `CliApp` can produce, what the user sees for each one, and how the "Did you mean" suggestions work.

Two types describe failures:

- `ValidationError` describes why the user's command-line input did not match the command. The parser produces it.
- `CliError[E]` is the error type of `CliApp.run`. It wraps a `ValidationError` or an error from your own code.

## ValidationError

`ValidationError` is defined in `zio.cli`:

```scala
final case class ValidationError(validationErrorType: ValidationErrorType, error: HelpDoc) extends Throwable
```

- `validationErrorType` is a short code for the kind of failure, such as `MissingValue` or `MissingFlag`.
- `error` is the text to show the user, as a `HelpDoc`.

`ValidationError` extends `Throwable`, but zio-cli never throws it. Parsers return it as a failure in the ZIO effect (`ZIO.fail`), so you handle it with the ZIO error channel. Common `validationErrorType` values include:

| Value | Meaning |
| --- | --- |
| `MissingValue` | An option was given without its value, or a required option is missing. |
| `MissingFlag` | The input contains a flag that the command does not define. |
| `InvalidValue` | A value could not be converted to the option's type. |
| `InvalidArgument` | A positional argument could not be converted to its type. |
| `CorrectedFlag` | A flag is close to a defined flag. The error suggests the correct name. See [Auto-correct suggestions](#auto-correct-suggestions). |

The parser also uses a few other values internally. You do not need to match on them.

`ValidationError.isOptionMissing` returns `true` when `validationErrorType` is `MissingValue`.

## CliError

`CliApp.run` returns `ZIO[R, CliError[E], Option[A]]`. `CliError` is a sealed trait with four cases:

```scala mdoc:silent
import zio._
import zio.cli._

def report(error: CliError[String]): UIO[Unit] =
  error match {
    case CliError.Parsing(e)     => Console.printLineError(s"Input rejected: ${e.validationErrorType}").orDie
    case CliError.BuiltIn(e)     => Console.printLineError(s"Built-in option failed: ${e.getMessage}").orDie
    case CliError.IO(e)          => Console.printLineError(s"Could not write output: ${e.getMessage}").orDie
    case CliError.Execution(msg) => Console.printLineError(s"Command failed: $msg").orDie
  }
```

| Case | Raised when | What the user sees |
| --- | --- | --- |
| `Parsing(e: ValidationError)` | The input does not match the command. | The text in `e.error` is printed to standard output before the failure is returned. |
| `BuiltIn(e: Exception)` | Generating shell completions fails. | Nothing is printed by zio-cli. The failure is returned to the caller. |
| `IO(e: IOException)` | Writing the help text, the banner, or the completion script to standard output fails. | Nothing is printed by zio-cli. The failure is returned to the caller. |
| `Execution[E](e: E)` | Your `execute` function fails with `E`. | zio-cli adds no output. Your error is passed through unchanged. |

`CliError` extends `Exception`, and its `getMessage` is always `"zio-cli app failed"`. Use the case itself, or `getCause`, to find the real reason. `getCause` returns the wrapped exception, or `null` for an `Execution` whose value is not a `Throwable`.

Built-in commands such as `--help` and `--wizard` are described in [Built-in commands](built-in-commands.md).

### Exit codes

`CliApp` does not set exit codes. It only returns the `CliError` value.

The `ZIOCli` trait (`zio-cli/shared/src/main/scala/zio/cli/ZIOCli.scala`) does set one. It catches `CliError.Parsing` and exits with `ExitCode.failure`, which is code 1. The other three cases are not caught by `ZIOCli`, so they reach ZIO as unhandled failures. The exit code for those depends on ZIO's default behavior. This page does not verify that default.

If you do not use `ZIOCli`, you choose the exit code yourself. For example, you can map each case to an `ExitCode` in your own `ZIOAppDefault`.

## Auto-correct suggestions

When a user types a flag that is almost right, zio-cli suggests the correct name. Here is an example with the default configuration:

```text
The flag "--bran" is not recognized. Did you mean --branch?
```

The suggestion is controlled by `autoCorrectLimit` in [CLI configuration](cli-config.md). The default value is 2. The suggestion is made only when all of these are true:

- The distance between the typed flag and the defined flag, counted as Levenshtein edits (inserts, deletes, and replacements), is at most `autoCorrectLimit`.
- The option's own name is longer than `autoCorrectLimit + 1` characters (`zio-cli/shared/src/main/scala/zio/cli/Options.scala`, line 453).

Auto-correct only suggests. It never rewrites the input. zio-cli does not run the command with the corrected flag. The parse fails with a `CorrectedFlag` error, and the user has to type the command again.

The suggestion is shown only when the rest of the input matched no other option (`Options.scala`, lines 229 to 235). If other options matched, the unknown flag is skipped and parsing continues.

## See also

- [CLI configuration](cli-config.md) for `autoCorrectLimit` and the other parser settings.
- [Commands](commands.md) for how commands are built.
- [Options](options.md) for the option types that produce `MissingValue` and `InvalidValue` errors.
