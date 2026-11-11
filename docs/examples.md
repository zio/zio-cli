---
id: examples
title: "Tutorial: build a git-like CLI"
---

This tutorial builds a small command-line program that works like a few parts of `git`. You will run it from a terminal, the way you run `git add` or `git remote add`. The program only prints what it would do. It does not change any files.

A **command-line interface (CLI)** is a program that reads its input from the words you type after its name. zio-cli is a Scala library that reads those words for you, checks them, and hands your code clean values. You describe the shape of the input, and zio-cli does the parsing.

The tutorial uses four building blocks:

- An **option** is a named switch, such as `-m`. It is either present or absent.
- An **argument** is a value given by its position, such as the directory `./src`.
- A **command** is a named action, such as `add`. It holds options and arguments.
- A **subcommand** is a command that sits under another command, such as `remote` under `git`.

You need a Scala project with sbt and the zio-cli dependency:

```scala
libraryDependencies += "dev.zio" %% "zio-cli" % "<version>"
```

Replace `<version>` with the zio-cli release you use. Each step below adds one idea. The complete program is at the end of the page.

## Step 1: Define one option and one argument

An option and an argument are the two kinds of input your program reads. Each one has a Scala type that describes the value it produces:

- `Options.boolean("m")` reads the `-m` switch. It produces a `Boolean`. The value is `true` when `-m` is present and `false` when it is not.
- `Args.directory("directory")` reads one positional value that must name a directory. It produces a `java.nio.file.Path`. The default does not require the directory to exist.

The name you pass in (`"m"`, `"directory"`) is what appears in help text. It is not the Scala variable name.

```scala mdoc:silent
import zio.cli._

import java.nio.file.{Path => JPath}

val modified: Options[Boolean] = Options.boolean("m")

val directory: Args[JPath] = Args.directory("directory")
```

## Step 2: Build a command

A command gives a name to a set of options and arguments. `Command("add", modified, directory)` means: the word `add`, then optionally `-m`, then one directory.

The type of the result is `Command[(Boolean, JPath)]`. zio-cli puts the option values and the argument values in a tuple, in the order you listed them. `withHelp` attaches the text that appears in the help output.

```scala mdoc:silent:reset
import zio.cli._

import java.nio.file.{Path => JPath}

val modified: Options[Boolean] = Options.boolean("m")
val directory: Args[JPath]     = Args.directory("directory")

val add: Command[(Boolean, JPath)] =
  Command("add", modified, directory).withHelp("Stage the files in a directory")
```

## Step 3: Add subcommands

A program usually has more than one action. The usual pattern is a root command, here `git`, with subcommands under it. Each subcommand is one action.

Two ideas are new in this step:

- `map` changes a command's result type. The tuple from Step 2 is turned into a value of a type you define. With one type for every action, the program can use a single `match` to decide what to do.
- `subcommands` attaches child commands to a parent. `Command("git").subcommands(add, remote)` accepts `git add ...` and `git remote ...`.

```scala mdoc:silent:reset
import zio.cli._

import java.nio.file.{Path => JPath}

sealed trait Subcommand extends Product with Serializable
object Subcommand {
  final case class Add(modified: Boolean, directory: JPath) extends Subcommand
  case object Remote                                       extends Subcommand
}

val modified: Options[Boolean] = Options.boolean("m")
val directory: Args[JPath]     = Args.directory("directory")

val add: Command[Subcommand] =
  Command("add", modified, directory)
    .withHelp("Stage the files in a directory")
    .map { case (modified, directory) => Subcommand.Add(modified, directory) }

val remote: Command[Subcommand] =
  Command("remote").withHelp("Manage the set of remotes").as(Subcommand.Remote)

val git: Command[Subcommand] =
  Command("git").subcommands(add, remote)
```

`Subcommand` is a `sealed trait`. The compiler then knows every possible case, so a `match` on it can be checked for completeness.

## Step 4: Add a nested subcommand

A subcommand can have its own subcommands. `git remote add <name> <url>` has three levels: the program `git`, the subcommand `remote`, and the sub-subcommand `add`.

In this step, `remote` stops being a plain command and becomes a parent. Its children are `add` and `remove`. The two children need their own result type, so the code puts them in a nested `Remote` group inside `Subcommand`.

- `Args.text("name") ++ Args.text("url")` reads two text values in order. `++` joins two arguments into one tuple.
- `Args.text` reads one text value. The name is only shown in help.

```scala mdoc:silent:reset
import zio.cli._

import java.nio.file.{Path => JPath}

sealed trait Subcommand extends Product with Serializable
object Subcommand {
  final case class Add(modified: Boolean, directory: JPath) extends Subcommand

  sealed trait Remote extends Subcommand
  object Remote {
    final case class Add(name: String, url: String) extends Remote
    final case class Remove(name: String)           extends Remote
  }
}

val modified: Options[Boolean] = Options.boolean("m")
val directory: Args[JPath]     = Args.directory("directory")

val add: Command[Subcommand] =
  Command("add", modified, directory)
    .withHelp("Stage the files in a directory")
    .map { case (modified, directory) => Subcommand.Add(modified, directory) }

val remoteAdd: Command[Subcommand.Remote] =
  Command("add", Args.text("name") ++ Args.text("url"))
    .withHelp("Add a remote with a name and a URL")
    .map { case (name, url) => Subcommand.Remote.Add(name, url) }

val remoteRemove: Command[Subcommand.Remote] =
  Command("remove", Args.text("name"))
    .withHelp("Remove the remote with this name")
    .map(name => Subcommand.Remote.Remove(name))

val remote: Command[Subcommand] =
  Command("remote")
    .withHelp("Manage the set of remotes")
    .subcommands(remoteAdd, remoteRemove)

val git: Command[Subcommand] =
  Command("git").subcommands(add, remote)
```

The parts fit together like this:

```text
git
├── add [-m] <directory>
└── remote
    ├── add <name> <url>
    └── remove <name>
```

## Step 5: Run the app

The program needs one more piece: a function that runs for each parsed command. `CliApp.make` takes the root command and that function. Put the complete program from the end of this page into a file in your project, then run it with sbt. The text after `run` is the input your program receives. The first word is always the root command name, `git`.

Show the help for the root command:

```shell
sbt "run git --help"
```

The output lists the commands and their help text. The layout below is approximate; your terminal output may differ in spacing:

```text
COMMANDS

  - add [-m] <directory>               Stage the files in a directory
  - remote add <name> <url>            Add a remote with a name and a URL
  - remote remove <name>               Remove the remote with this name
```

Run each subcommand with sample input:

```shell
sbt "run git add -m ./src"
```

```text
Executing `git add ./src` with modified flag set to true
```

```shell
sbt "run git add ./src"
```

```text
Executing `git add ./src` with modified flag set to false
```

```shell
sbt "run git remote add origin https://example.com/repo.git"
```

```text
Executing `git remote add origin https://example.com/repo.git`
```

```shell
sbt "run git remote remove origin"
```

```text
Executing `git remote remove origin`
```

## Step 6: What to try next

Each of these pages covers one part of the library in more detail:

- [Commands](commands.md) covers command trees, `map`, and `subcommands` in full.
- [Options](options.md) lists the option types, aliases, and boolean negation.
- [Args](args.md) lists the argument types and how to repeat them.
- [Help documentation](helpdoc.md) explains how to write help text with `HelpDoc`.
- [Testing CLI applications](testing.md) shows how to test a program without a terminal.

Some small changes to try on this program:

1. Add a short alias for `-m` with `Options.boolean("modified").alias("m")`, so `--modified` works too.
2. Add a `remote list` subcommand that prints a line such as "Executing `git remote list`" (a `Command("list")` under `remote`, plus a new case in the `Subcommand.Remote` group and in the final `match`).
3. Change `remoteAdd` to read the name and URL as options, and compare the help output.

## Complete program

This is the whole program in one block. It matches the steps above.

```scala mdoc:compile-only
import zio.Console.printLine
import zio.cli.HelpDoc.Span.text
import zio.cli._

import java.nio.file.{Path => JPath}

object GitTutorial extends ZIOCliDefault {

  sealed trait Subcommand extends Product with Serializable
  object Subcommand {
    final case class Add(modified: Boolean, directory: JPath) extends Subcommand

    sealed trait Remote extends Subcommand
    object Remote {
      final case class Add(name: String, url: String) extends Remote
      final case class Remove(name: String)           extends Remote
    }
  }

  val modified: Options[Boolean] = Options.boolean("m")
  val directory: Args[JPath]     = Args.directory("directory")

  val add: Command[Subcommand] =
    Command("add", modified, directory)
      .withHelp("Stage the files in a directory")
      .map { case (modified, directory) => Subcommand.Add(modified, directory) }

  val remoteAdd: Command[Subcommand.Remote] =
    Command("add", Args.text("name") ++ Args.text("url"))
      .withHelp("Add a remote with a name and a URL")
      .map { case (name, url) => Subcommand.Remote.Add(name, url) }

  val remoteRemove: Command[Subcommand.Remote] =
    Command("remove", Args.text("name"))
      .withHelp("Remove the remote with this name")
      .map(name => Subcommand.Remote.Remove(name))

  val remote: Command[Subcommand] =
    Command("remote")
      .withHelp("Manage the set of remotes")
      .subcommands(remoteAdd, remoteRemove)

  val git: Command[Subcommand] =
    Command("git").subcommands(add, remote)

  val cliApp = CliApp.make(
    name = "Git Version Control",
    version = "0.9.2",
    summary = text("a client for the git dvcs protocol"),
    command = git
  ) {
    case Subcommand.Add(modified, directory) =>
      printLine(s"Executing `git add $directory` with modified flag set to $modified")
    case Subcommand.Remote.Add(name, url) =>
      printLine(s"Executing `git remote add $name $url`")
    case Subcommand.Remote.Remove(name) =>
      printLine(s"Executing `git remote remove $name`")
  }
}
```
