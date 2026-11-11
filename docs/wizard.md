---
id: wizard
title: "Wizard mode"
---
Wizard mode is an interactive way to build a command. Instead of typing the full command line, the user answers questions in the terminal. The app asks which subcommand to run, then asks for each value the command needs. When every answer is in, the app prints the complete command line and runs it.

Wizard mode is useful when users do not know the options and arguments of an app, or when they want a guided way to try a command. Scripts and experienced users usually type the full command instead.

## Starting wizard mode

Wizard mode is a built-in option, so every app has it. Add `--wizard` after the command name. For the app in [Getting started](getting-started.md), run:

```
sbt "run --wizard"
```

The option is defined in `zio-cli/shared/src/main/scala/zio/cli/BuiltInOption.scala`. `Command.scala` turns it into a wizard session for the command the user named. See [Built-in commands](built-in-commands.md) for the full list of built-in options.

The short form `-w` also starts the wizard, through a fallback in `Command.scala` that runs when the command line is not valid. The examples on this page use `--wizard`.

## How a session runs

A session has these steps. The app prints a short header first. It names the app and explains that wizard mode is running.

1. **Choose a subcommand.** If the command has more than one subcommand, the wizard prints a numbered list of them, each with its short description. Type the name of a subcommand, or type its number. If the command has only one subcommand, the wizard selects it without asking.
2. **Answer each argument and option.** For each value the chosen command needs, the wizard prints a short description of it and waits for input. The input must be valid for that value. If it is not, the wizard asks again.
3. **Review the result.** The wizard prints the values it collected as one line, so the user can see what to type to run the same command without the wizard. It then prints a line that says the command is executing, and runs it.

Every question uses a prompt. The prompt is the name of the command followed by ` :> `. For a command named `greet`, the prompt is `greet :> `. The name comes from the first name of the command (`Wizard.scala`, the `prompt` value).

## Special input words

At any question, three words have a special meaning. The wizard compares your input to these words as exact strings. The match is case-sensitive and does not trim spaces, so `Help`, `QUIT` and ` quit` are not special. Those inputs are treated as ordinary input and fail validation.

- `help` shows the description of the current question in more detail. It does not change your answers, and the question is asked again.
- `restart` throws away your answers and starts the session again from the first question.
- `quit` leaves wizard mode. The command is not run.

Empty input, which means pressing enter with nothing typed, is not accepted. The wizard shows a message that no text was found and asks the same question again.

## Choosing by number

When the wizard lists subcommands, you can type either the name or the number shown next to it. A number that is not in the list, such as `0` or a number larger than the list, is rejected. The wizard asks the question again and shows what was wrong. Text that is neither a name nor a number is rejected in the same way.

## The final command

When all values are collected, the wizard prints the values as one line. The line holds the subcommand names, options and arguments that were chosen, in the order the wizard asked for them. It does not include the app's own command name, because the app adds that name itself when it runs the command. The wizard then runs the command with the values you chose.

For example, if the answers for the app in [Getting started](getting-started.md) were the option `--loud` and the name `Ada`, the printed line would hold `--loud Ada`. To skip the wizard next time, pass the same values after the app name, as in `sbt "run --loud Ada"`.

## Which values the wizard asks for

The wizard asks for arguments and for options that have a value to type or a default. Options that are required and have no default, such as an option created with `Options.text`, are not asked for. The wizard does not ask for them, so the final command will not contain them. Use an option with a default, such as `Options.boolean`, when you want the wizard to ask for it. This behavior comes from the code in `Wizard.scala` and `Options.scala`.

## A minimal example

The following app has one command with one argument. Starting it with `--wizard` asks for the name:

```scala mdoc:compile-only
import zio.cli._
import zio.cli.HelpDoc.Span.text
import zio.Console.printLine

val greet = Command("greet", Args.text("name"))

val cliApp = CliApp.make(
  name = "greet",
  version = "0.1.0",
  summary = text("Prints a greeting"),
  command = greet
) { name =>
  printLine(s"Hello, $name!")
}

// Starts wizard mode. This reads from the terminal, so it is not run when the docs are built.
cliApp.run(List("--wizard"))
```

An app with subcommands shows the numbered list instead:

```scala mdoc:compile-only
import zio.cli._
import zio.cli.HelpDoc.Span.text
import zio.Console.printLine

val repo = Command("repo").subcommands(
  Command("clone", Args.text("repository")),
  Command("add", Args.text("path"))
)

val cliApp = CliApp.make(
  name = "repo",
  version = "0.1.0",
  summary = text("Manages repositories"),
  command = repo
) { value =>
  printLine(s"Selected: $value")
}

// Starts wizard mode. The user picks clone or add from the numbered list.
cliApp.run(List("--wizard"))
```

After a wizard session, the same values can be passed on the command line, starting with the subcommand name, for example `clone` followed by the repository.

## Related pages

- [Built-in commands](built-in-commands.md) lists `--wizard` with `--help` and shell completion.
- [Commands](commands.md) explains how subcommands are built.
- [Args](args.md) and [Options](options.md) explain the values that the wizard asks for.
