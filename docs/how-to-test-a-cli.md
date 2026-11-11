---
id: how-to-test-a-cli
title: "How to test a CLI application"
---

This guide shows how to write a [ZIO Test](https://zio.dev/reference/test/) spec for a **ZIO CLI** command. The spec checks three things:

1. **Parsing**: valid arguments produce the right value, and invalid arguments produce an error.
2. **Help output**: `--help` returns the help directive, and the help text lists the options.
3. **Failure behaviour**: a parse error and an error from the app's own code each produce the expected error type.

A **spec** is a ZIO Test class that holds tests. A **suite** groups tests under a name. Each **test** runs one check. The checks use the assertions from the [testkit reference](testing.md).

## Prerequisites

- A `Command` or `CliApp` to test. This guide uses a small `greet` command.
- The testkit on the test classpath. See [Setup](testing.md#setup).

## Step 1: Define the command and the app

The first block defines the command under test. It has two required options, `--name` (text) and `--times` (integer). The app prints a greeting. A second app, `failingApp`, always fails, so you can test the execution error path.

```scala mdoc:silent:reset
import zio.ZIO
import zio.cli._
import zio.cli.testkit._
import zio.cli.testkit.CliAssertion._
import zio.test._

object GreetCli {

  val greet: Command[(String, BigInt)] =
    Command("greet", Options.text("name") ++ Options.integer("times"))

  val app: CliApp[Any, Nothing, String] =
    CliApp.make("greet", "1.0.0", HelpDoc.Span.text("Greets a person."), greet) { case (name, times) =>
      ZIO.succeed(s"Hello, $name x$times")
    }

  val failingApp: CliApp[Any, String, String] =
    CliApp.make[Any, String, (String, BigInt), String](
      "greet",
      "1.0.0",
      HelpDoc.Span.text("Greets a person."),
      greet
    ) { case (_, _) =>
      ZIO.fail("boom")
    }
}
```

The argument list you pass to a `Command` starts with the command name. The argument list you pass to a `CliApp` does not, because `CliApp.run` adds the name itself. That is why the command tests in later steps start with `"greet"` and the app tests do not. See [Commands](commands.md).

The `import zio.cli.testkit.CliAssertion._` line brings the implicit `CliConfig` into scope. The assertions need it.

## Step 2: Test parsing

Use `assertCommand` when a valid input must produce a value. Each `CliRepr` holds the arguments and the expected value. Use `assertCommandSuccess` when an input must produce a `ValidationError`. Its `Assertion` receives the whole `Either[ValidationError, A]`.

```scala mdoc:silent
val validInputs: Gen[Any, CliRepr[List[String], (String, BigInt)]] =
  Gen.fromIterable(
    List(
      CliRepr(List("greet", "--name", "Ada", "--times", "2"), ("Ada", BigInt(2))),
      CliRepr(List("greet", "--times", "1", "--name", "Lin"), ("Lin", BigInt(1)))
    )
  )

val invalidInputs: Gen[Any, CliRepr[List[String], Assertion[Either[ValidationError, (String, BigInt)]]]] =
  Gen.fromIterable(
    List(
      // --times is required and missing
      CliRepr(
        List("greet", "--name", "Ada"),
        Assertion.assertion[Either[ValidationError, (String, BigInt)]]("is a validation error") {
          case Left(_) => true
          case _       => false
        }
      ),
      // --times is not an integer
      CliRepr(
        List("greet", "--name", "Ada", "--times", "two"),
        Assertion.assertion[Either[ValidationError, (String, BigInt)]]("is a validation error") {
          case Left(_) => true
          case _       => false
        }
      )
    )
  )

val parsingTests =
  suite("parsing")(
    test("valid arguments produce the parsed values") {
      assertCommand(GreetCli.greet, validInputs)
    },
    test("invalid arguments produce a validation error") {
      assertCommandSuccess(GreetCli.greet, invalidInputs)
    }
  )
```

`assertCommand` and `assertCommandSuccess` each return a ZIO effect. Returning that effect from `test` is enough. The check draws inputs from the generator and fails if any drawn input does not match.

## Step 3: Test help output

There are two ways to check help. Use `assertCommandAll` to check the built-in directive that the command returns. Use `outputContains` to check the text the app prints.

`--help` does not produce a value of the command's type. `assertCommandAll` receives the `TestReturn` result, so a built-in option is a `TestReturn.Help()` value.

```scala mdoc:silent
val helpInput: CliRepr[List[String], Assertion[Either[ValidationError, TestReturn[(String, BigInt)]]]] =
  CliRepr(
    List("greet", "--help"),
    Assertion.assertion[Either[ValidationError, TestReturn[(String, BigInt)]]]("is the help directive") {
      case Right(TestReturn.Help()) => true
      case _                        => false
    }
  )

val helpTests =
  suite("help")(
    test("--help returns the built-in help directive") {
      assertCommandAll(GreetCli.greet, Gen.const(helpInput))
    },
    test("--help prints the option names") {
      outputContains(GreetCli.app, Gen.const(CliRepr(List("--help"), Seq("--name", "--times"))))
    }
  )
```

`outputContains` checks the console output that ZIO Test captures. It passes when each word appears in one printed line. It uses the option names, because those names appear in the synopsis line of the help output.

## Step 4: Test failure behaviour

Use `assertCliApp` for failures. It receives the full `Either[CliError[E], A]` from `CliApp.run`. There are two kinds of failure:

- `CliError.Parsing` means the arguments did not parse. The app prints the help text and stops before it runs its code.
- `CliError.Execution` means the app's own code failed. Its value is the error the app returned.

```scala mdoc:silent
val parseFailure: Gen[Any, CliRepr[List[String], Assertion[Either[CliError[Nothing], String]]]] =
  Gen.const(
    CliRepr(
      List("--name", "Ada"),
      Assertion.assertion[Either[CliError[Nothing], String]]("is a parsing error") {
        case Left(CliError.Parsing(_)) => true
        case _                         => false
      }
    )
  )

val executionFailure: Gen[Any, CliRepr[List[String], Assertion[Either[CliError[String], String]]]] =
  Gen.const(
    CliRepr(
      List("--name", "Ada", "--times", "2"),
      Assertion.assertion[Either[CliError[String], String]]("is an execution error with boom") {
        case Left(CliError.Execution("boom")) => true
        case _                                => false
      }
    )
  )

val failureTests =
  suite("failures")(
    test("missing --times is a parsing error") {
      assertCliApp(GreetCli.app, parseFailure)
    },
    test("an error from the app is an execution error") {
      assertCliApp(GreetCli.failingApp, executionFailure)
    }
  )
```

The `failingApp` value in Step 1 always fails with `"boom"`. The `executionFailure` check expects `Left(CliError.Execution("boom"))`. Use `assertCliAppValues` instead when the app should succeed and you only want to compare the returned value.

Do not use `assertCliAppZIO` for parse errors. It passes parse errors through as a failure of the whole check, not as an `Assertion` result. See [CliApp assertions](testing.md#cliapp-assertions).

## Step 5: Assemble the spec

Put the suites in one spec object. Give the spec a name that matches the command.

```scala mdoc:silent
object GreetCliSpec extends ZIOSpecDefault {
  def spec =
    suite("GreetCli")(parsingTests, helpTests, failureTests)
}
```

Run the spec with the test task of your build, for example `sbt test`.

## Checklist

- Each command test passes the command name as the first argument. Each app test does not.
- Each test that uses an assertion has the testkit's `CliAssertion._` import, which provides the implicit `CliConfig`.
- Help checks use `assertCommandAll` for the directive and `outputContains` for the printed text.
- Parse errors use `assertCommandSuccess` on a command, or `assertCliApp` on an app. Do not use `assertCliAppZIO` for them.

## See also

- [Testing CLI applications](testing.md) lists every testkit API with its signature.
- [Commands](commands.md) and [Options](options.md) describe the values under test.
- [Built-in commands](built-in-commands.md) describes `--help`, shell completion and the wizard.
