---
id: testing
title: "Testing CLI applications"
---

The `zio-cli-testkit` module helps you test **ZIO CLI** commands and applications with [ZIO Test](https://zio.dev/reference/test/). It provides:

- assertions that check the usage synopsis and help text of a command,
- assertions that check how a command parses its arguments,
- assertions that check what a `CliApp` returns and what it prints,
- generators that build many test inputs for one check (property-based testing).

An **assertion** is a check that either passes or fails. A **generator** (`Gen`) is a ZIO Test value that produces many sample inputs. For a step-by-step example, see [How to test a CLI application](how-to-test-a-cli.md).

## Setup

The testkit depends on `zio-test`. Its build definition sets `skip / publish := true`, so the module is not published as a separate artifact. Use it from this repository's `zio-cli-testkit` project.

Import the public types and assertions:

```scala
import zio.cli.testkit._                     // CliRepr, TestReturn, Tree, and the two objects
import zio.cli.testkit.CliAssertion._        // assertion methods and the implicit CliConfig
```

The assertion methods take an implicit `CliConfig`. `CliAssertion` defines one (`CliConfig.default`), so you must import it. Without the import, the compiler reports a missing implicit `cliConfig`. Do not import `CliAssertion._` and `GenerationAssertion._` together. Both objects define a member named `cliConfig`, so the two wildcard imports clash. Import one object, or import only the `cliConfig` you need.

## Two kinds of input

The testkit works with two inputs. Each uses a different argument list.

| Input | Argument list | Example |
| --- | --- | --- |
| `Command[A]` | First element is the command name | `List("greet", "--name", "Ada")` |
| `CliApp[R, E, A]` | No command name; `CliApp` adds it | `List("--name", "Ada")` |

A `Command` is parsed directly, so the name must be the first element. A `CliApp` adds the name itself before it parses the arguments. See [Commands](commands.md) and [Options](options.md) for how these are built.

## CliAssertion

`CliAssertion` is the object that holds most assertions. Its methods fall into five groups.

### Access methods

These methods read a part of a `CliApp` and pass it to an `Assertion` you write. Use them when no other method fits.

```scala
def accessCommand[R, E, A](cliApp: CliApp[R, E, A], assert: Assertion[Command[_]]): TestResult
def accessHelpDoc[R, E, A](cliApp: CliApp[R, E, A], assert: Assertion[HelpDoc]): TestResult
def accessSynopsis[R, E, A](cliApp: CliApp[R, E, A], assert: Assertion[UsageSynopsis]): TestResult
```

- `accessCommand` passes the `Command` inside the app.
- `accessHelpDoc` passes the `HelpDoc` of that command.
- `accessSynopsis` passes the `UsageSynopsis` of that command.

Example:

```scala mdoc:silent:reset
import zio._
import zio.cli._
import zio.cli.testkit.CliAssertion
import zio.test._

val greet: Command[String] = Command("greet", Options.text("name"))

val app: CliApp[Any, Nothing, String] =
  CliApp.make("greet", "1.0.0", HelpDoc.Span.text("Greets a person."), greet)(name => ZIO.succeed(name))

val hasGreetName: TestResult =
  CliAssertion.accessCommand(app, Assertion.assertion[Command[_]]("has the name greet")(_.names.contains("greet")))
```

### Synopsis and help assertions

```scala
def assertSynopsis[A](command: Command[A], synopsis: String)(implicit cliConfig: CliConfig): TestResult
def assertSynopsis[A](command: Command[A], synopsis: List[String])(implicit cliConfig: CliConfig): TestResult
def assertSynopsis[A](command: Command[A], synopsis: UsageSynopsis): TestResult
def assertSynopsis[R, E, A](cliApp: CliApp[R, E, A], synopsis: String)(implicit cliConfig: CliConfig): TestResult
def assertSynopsis[R, E, A](cliApp: CliApp[R, E, A], synopsis: List[String])(implicit cliConfig: CliConfig): TestResult
def assertSynopsis[R, E, A](cliApp: CliApp[R, E, A], synopsis: UsageSynopsis): TestResult

def assertHelpDoc[A](command: Command[A], helpDoc: HelpDoc): TestResult
def assertHelpDoc[R, E, A](cliApp: CliApp[R, E, A], helpDoc: HelpDoc): TestResult
```

- `assertSynopsis` checks the usage line or lines of a command. The `String` form expects one line. The `List[String]` form expects each line in order. The `UsageSynopsis` form compares a synopsis value built in code.
- `assertHelpDoc` checks the help document of a command. It compares the HTML rendering of both values.

Example:

```scala mdoc:silent:reset
import zio.cli._
import zio.cli.testkit.CliAssertion._
import zio.test._

val greet: Command[(String, BigInt)] =
  Command("greet", Options.text("name") ++ Options.integer("times"))

val synopsisCheck: TestResult =
  assertSynopsis(greet, "greet --name <text> --times <integer>")

val helpCheck: TestResult =
  assertHelpDoc(greet, greet.helpDoc)
```

### Parsing assertions

These methods run `Command.parse` on each generated input. They take a `Gen` of `CliRepr` values. Each `CliRepr` holds the arguments and the expected result.

```scala
def assertCommand[A, R](
  command: Command[A],
  pairs: Gen[R, CliRepr[List[String], A]]
)(implicit cliConfig: CliConfig): ZIO[R, Throwable, TestResult]

def assertCommandSuccess[A, R](
  command: Command[A],
  pairs: Gen[R, CliRepr[List[String], Assertion[Either[ValidationError, A]]]]
)(implicit cliConfig: CliConfig): ZIO[R, Throwable, TestResult]

def assertCommandAll[A, R](
  command: Command[A],
  pairs: Gen[R, CliRepr[List[String], Assertion[Either[ValidationError, TestReturn[A]]]]]
)(implicit cliConfig: CliConfig): ZIO[R, Throwable, TestResult]
```

- `assertCommand` expects the parsed value to equal the value in the `CliRepr`. A `ValidationError` makes the check fail.
- `assertCommandSuccess` gives the `Assertion` the full `Either[ValidationError, A]`. Use it to check for a parse error.
- `assertCommandAll` gives the `Assertion` the full `Either[ValidationError, TestReturn[A]]`. Use it when the input is a built-in option such as `--help`, because a built-in option does not produce a value of type `A`. See [TestReturn](#testreturn).

Example with a successful parse and a parse error:

```scala mdoc:silent:reset
import zio._
import zio.cli._
import zio.cli.testkit._
import zio.cli.testkit.CliAssertion._
import zio.test._

val greet: Command[(String, BigInt)] =
  Command("greet", Options.text("name") ++ Options.integer("times"))

val validInputs: Gen[Any, CliRepr[List[String], (String, BigInt)]] =
  Gen.fromIterable(List(CliRepr(List("greet", "--name", "Ada", "--times", "2"), ("Ada", BigInt(2)))))

val parses: ZIO[Any, Throwable, TestResult] = assertCommand(greet, validInputs)

val missingTimes: Gen[Any, CliRepr[List[String], Assertion[Either[ValidationError, (String, BigInt)]]]] =
  Gen.const(
    CliRepr(
      List("greet", "--name", "Ada"),
      Assertion.assertion[Either[ValidationError, (String, BigInt)]]("is a validation error") {
        case Left(_) => true
        case _       => false
      }
    )
  )

val rejects: ZIO[Any, Throwable, TestResult] = assertCommandSuccess(greet, missingTimes)
```

### CliApp assertions

These methods run `CliApp.run` on each generated input.

```scala
def assertCliApp[R, E, A, RGen](
  cliApp: CliApp[R, E, A],
  pairs: Gen[RGen, CliRepr[List[String], Assertion[Either[CliError[E], A]]]]
)(implicit cliConfig: CliConfig): ZIO[RGen with R, Throwable, TestResult]

def assertCliAppValues[R, E, A, RGen](
  cliApp: CliApp[R, E, A],
  pairs: Gen[RGen, CliRepr[List[String], A]]
)(implicit cliConfig: CliConfig): ZIO[RGen with R, Throwable, TestResult]

def assertCliAppZIO[R, E, A, RGen](
  cliApp: CliApp[R, E, A],
  pairs: Gen[RGen, CliRepr[List[String], ZIO[R, E, A]]]
): ZIO[RGen with R, Throwable, TestResult]
```

- `assertCliApp` gives the `Assertion` the full `Either[CliError[E], A]`. Use it for parse errors (`CliError.Parsing`) and for errors from the app itself (`CliError.Execution`).
- `assertCliAppValues` expects the app to succeed with the value in the `CliRepr`.
- `assertCliAppZIO` runs a ZIO effect that you supply as the expected result, and compares it with the app result. Only `CliError.Execution` values are compared. Any other `CliError` (for example a parse error) makes the check fail with that error instead of a test assertion result. Use `assertCliApp` for parse errors.

An app that prints help returns `None` from `run`. The `assertCliApp` family does not handle `None` and fails the check with a plain `Exception`. Test help output with [outputContains](#console-output) instead.

Example:

```scala mdoc:silent:reset
import zio._
import zio.cli._
import zio.cli.testkit._
import zio.cli.testkit.CliAssertion._
import zio.test._

val greet: Command[(String, BigInt)] =
  Command("greet", Options.text("name") ++ Options.integer("times"))

val app: CliApp[Any, Nothing, String] =
  CliApp.make("greet", "1.0.0", HelpDoc.Span.text("Greets a person."), greet) { case (name, times) =>
    ZIO.succeed(s"Hello, $name x$times")
  }

val returnsValue: Gen[Any, CliRepr[List[String], String]] =
  Gen.const(CliRepr(List("--name", "Ada", "--times", "2"), "Hello, Ada x2"))

val valueCheck: ZIO[Any, Throwable, TestResult] = assertCliAppValues(app, returnsValue)

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

val errorCheck: ZIO[Any, Throwable, TestResult] = assertCliApp(app, parseFailure)
```

### Console output

```scala
def outputContains[R, E, A, RGen](
  cliApp: CliApp[R, E, A],
  pairs: Gen[RGen, CliRepr[List[String], Seq[String]]]
): ZIO[RGen with R, Throwable, TestResult]
```

`outputContains` runs the app and reads the console output that ZIO Test captures with `TestConsole`. It passes when, for each `String` in the expected `Seq`, at least one output line contains that text. A word that is split across two lines does not match. Use it for help output, because `--help` makes `run` return `None` (see [CliApp assertions](#cliapp-assertions)).

Example:

```scala mdoc:silent:reset
import zio._
import zio.cli._
import zio.cli.testkit._
import zio.cli.testkit.CliAssertion._
import zio.test._

val greet: Command[(String, BigInt)] =
  Command("greet", Options.text("name") ++ Options.integer("times"))

val app: CliApp[Any, Nothing, String] =
  CliApp.make("greet", "1.0.0", HelpDoc.Span.text("Greets a person."), greet) { case (name, times) =>
    ZIO.succeed(s"Hello, $name x$times")
  }

val helpOutput: Gen[Any, CliRepr[List[String], Seq[String]]] =
  Gen.const(CliRepr(List("--help"), Seq("--name", "--times")))

val helpCheck: ZIO[Any, Throwable, TestResult] = outputContains(app, helpOutput)
```

## GenerationAssertion

`GenerationAssertion` tests a function that builds a `Command` or a `CliApp` from a seed value. A seed is any value you choose, such as a name. Each check has two levels of generators. The outer generator produces `(seed, innerGen)` pairs. The inner generator produces `CliRepr` values for the built command or app.

```scala
def commandCreation[A, B, R1, R2](
  f: A => Command[B],
  response: Gen[R1, (A, Gen[R2, CliRepr[List[String], B]])]
)(implicit cliConfig: CliConfig): ZIO[R1 with R2, Throwable, TestResult]

def commandCreationZIO[A, B, R, E <: Throwable, RGen1, RGen2](
  f: A => ZIO[R, E, Command[B]],
  response: Gen[RGen1, (A, Gen[RGen2, CliRepr[List[String], B]])]
): ZIO[R with RGen1 with RGen2, Throwable, TestResult]

def cliAppCreation[R, E, A, B, RGen](
  f: A => CliApp[R, E, B],
  response: Gen[RGen, (A, Gen[RGen, CliRepr[List[String], B]])]
): ZIO[R with RGen, Throwable, TestResult]

def cliAppCreationZIO[R, E, A, B, RGen, RC, EC <: Throwable](
  f: A => ZIO[RC, EC, CliApp[R, E, B]],
  response: Gen[RGen, (A, Gen[RGen, CliRepr[List[String], B]])]
): ZIO[R with RC with RGen, Throwable, TestResult]
```

- `commandCreation` and `commandCreationZIO` check each generated `Command` with `CliAssertion.assertCommand`. The `ZIO` variant builds the command in an effect first.
- `cliAppCreation` and `cliAppCreationZIO` check each generated `CliApp` with `CliAssertion.assertCliAppValues`.

Example with a command factory:

```scala mdoc:silent:reset
import zio._
import zio.cli._
import zio.cli.testkit._
import zio.cli.testkit.GenerationAssertion._
import zio.test._

def greetCommand(header: String): Command[String] =
  Command(header, Options.text("name")).map(v => s"$header: $v")

def greetPairs(header: String): Gen[Any, CliRepr[List[String], String]] =
  Gen.fromIterable(List("Ada", "Lin")).map(v => CliRepr(List(header, "--name", v), s"$header: $v"))

val seeds: Gen[Any, (String, Gen[Any, CliRepr[List[String], String]])] =
  Gen.fromIterable(List("hello", "bye")).map(h => (h, greetPairs(h)))

val commandsCheck: ZIO[Any, Throwable, TestResult] =
  commandCreation((h: String) => greetCommand(h), seeds)
```

## TestReturn

```scala
sealed trait TestReturn[+A]

object TestReturn {
  def convert[A](res: CommandDirective[A]): TestReturn[A]

  sealed trait BuiltIn extends TestReturn[Nothing]
  case class Help()   extends BuiltIn
  case class Wizard() extends BuiltIn
  case class SH()     extends BuiltIn
  case class SHH()    extends BuiltIn
  case class Value[A](a: A) extends TestReturn[A]
}
```

`TestReturn` is the result type that `assertCommandAll` checks. `convert` turns a `CommandDirective` into a `TestReturn`:

- A user-defined result becomes `Value(a)`.
- Any built-in directive becomes `Help()`. This includes `--wizard` and shell completion, which `convert` does not separate from help. The `Wizard`, `SH` and `SHH` cases exist in the type but `convert` never returns them.

Example:

```scala mdoc:silent:reset
import zio.cli._
import zio.cli.testkit.TestReturn

val fromValue: TestReturn[Int] = TestReturn.convert(CommandDirective.userDefined(Nil, 5))
val fromValueIsValue: Boolean = fromValue == TestReturn.Value(5)
```

## CliRepr

```scala
final case class CliRepr[+A, +R](value: A, repr: R) {
  def map[B](f: A => B): CliRepr[B, R]
  def map2[B](f: R => B): CliRepr[A, B]
}
```

`CliRepr` pairs an input with its expected result. `value` is the input (usually the argument list). `repr` is the expected result (a value, a `ZIO` effect, or an `Assertion`). `map` changes `value`, and `map2` changes `repr`. Use `map2` to turn a plain expected value into an `Assertion` or an effect.

Example:

```scala mdoc:silent:reset
import zio._
import zio.cli.testkit.CliRepr

val input: CliRepr[List[String], Int] = CliRepr(List("--times", "2"), 2)
val argCount: CliRepr[Int, Int] = input.map(_.size)
val effect: CliRepr[List[String], UIO[Int]] = input.map2(n => ZIO.succeed(n))
```

## Tree

```scala
final case class Tree[A](
  atoms: List[Gen[Any, A]],
  mappers: List[Tree.Mapper[A, Any]],
  operators: List[((A, A) => A)],
  depth: Int
) {
  def gen: Gen[Any, A]
  lazy val anyAtom: Gen[Any, A]
  def anyTree(depth: Int): Gen[Any, A]
}

object Tree {
  final case class Compare[A, Repr](a: A, repr: Repr)
  final case class Mapper[A, B](f: (A, B) => A, anyB: Gen[Any, B])
}
```

`Tree` builds a generator of nested values, such as expressions. Atoms are leaf values. Operators combine two sub-trees. Mappers combine one sub-tree with a value from a second generator.

- `gen` picks a nesting depth between 0 and 2 and builds a value of that depth.
- `anyTree(depth)` builds values of the depth you pass.
- The `depth` field is stored but `gen` does not read it. `gen` always uses a depth between 0 and 2.
- `Tree.Compare` is a helper type. The testkit does not use it.

Example:

```scala mdoc:silent:reset
import zio.test.Gen
import zio.cli.testkit.Tree

val arithmetic: Tree[Int] = Tree[Int](
  atoms = List(Gen.const(1), Gen.const(2), Gen.const(3)),
  mappers = List(Tree.Mapper[Int, Any]((a: Int, _: Any) => a + 1, Gen.const[Any](()))),
  operators = List((a: Int, b: Int) => a + b, (a: Int, b: Int) => a * b),
  depth = 2
)

val expressions: Gen[Any, Int] = arithmetic.gen
```

## See also

- [How to test a CLI application](how-to-test-a-cli.md) walks through a complete test spec.
- [Commands](commands.md) and [Options](options.md) describe the values you test.
- [HelpDoc](helpdoc.md) describes the help document that `assertHelpDoc` compares.
- [CLI config](cli-config.md) describes `CliConfig`, which the assertions take as an implicit.
