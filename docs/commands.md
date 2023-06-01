---
id: commands
title: "Commands"
---

`Command[Model]` is a description of a CLI command. This is parsed by `CliApp` to produce a collection of valid commands and transform a valid input from a user into an instance of type `Model`. Then you can implement the functionality of the CLI App using pattern matching on instances of `Model`.



It's possible to create a `Command` using the `apply` method. You must always specify the name of the command that will be used
in the CLI, but you can also specify options and/or arguments:

```scala mdoc:silent
import zio.cli._

object Command {
  def apply(name: String) = ???
  def apply[ArgsType](name: String, args: Args[ArgsType]) = ???
  def apply[OptionsType](name: String, options: Options[OptionsType]) = ???
  def apply[OptionsType, ArgsType](name: String, options: Options[OptionsType], args: Args[ArgsType]) = ???
}
```
## Combining commands and transformations
**ZIO CLI** also allows to combine different commands in a functional manner.

```scala mdoc:silent
trait Command[+A] {
  def map[B](f: A => B): Command[B]
  def |[A1 >: A](that: Command[A1]): Command[A1]
  def orElse[A1 >: A](that: Command[A1]): Command[A1]
  def orElseEither[B](that: Command[B]): Command[Either[A, B]]
  def withHelp(help: HelpDoc): Command[A]
  def withHelp(help: String): Command[A]
  def subcommands[B](that: Command[B])
  def subcommands[B](c1: Command[B], c2: Command[B], cs: Command[B]*)
}
```
### Example
In the following example, we are going to construct a command using some of the methods above
```scala mdoc:silent:reset
import zio.cli._

// Construction of basic commands and HelpDoc
val parentCommand: Command[BigInt] = Command("pC", Options.integer("int"))
val childCommand1: Command[String] = Command("cC1", Options.text("text1"))
val childCommand2: Command[String] = Command("cC2", Options.text("text2"))
val help = "This is a command with parent and children"

// Create a command with two possible subcommands
val finalCommand = parentCommand.subcommands(childCommand1, childCommand2)

// Add help to command
val finalCommandWithHelp = finalCommand.withHelp(help)

```

## Customizing the type parameter 
`Command[_]` is a generic class. We can apply `map` to construct commands returning any custom type that helps us implement business logic. 
```scala mdoc:silent
case class CustomType(int: BigInt, text: String)

val customCommand: Command[CustomType] =
  finalCommandWithHelp.map {
    case (int, text) => CustomType(int, text)
  }
```

Now we can construct a `Command[CustomType]` so we can implement the business logic of our app using directly `CustomType` instead of a tuple. This can be of great use if we have employed various (`orElseEither`):
```scala mdoc:silent
import java.time.{ LocalDate => JLocalDate }

sealed trait Value
case class TextV(text: String) extends Value
case class IntV(int: BigInt) extends Value
case class DateV(date: JLocalDate) extends Value

val options: Options[Either[Either[String, BigInt], JLocalDate]] = Options.text("opt1") orElseEither Options.integer("opt2") orElseEither Options.localDate("opt3")
val command: Command[Either[Either[String, BigInt], JLocalDate]] = Command("command", options)
val prettyCommand: Command[Value] = command.map {
  case Left(Left(x)) => TextV(x)
  case Left(Right(x)) => IntV(x)
  case Right(x) => DateV(x)
}
```
In this example, we have transformed a rather cumbersome `Command[Either[Either[String, BigInt], JLocalDate]]` into a `Command[Value]`.


