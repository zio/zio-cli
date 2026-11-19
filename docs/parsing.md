---
id: parsing
title: "How ZIO CLI reads a command line"
sidebar_label: "How Parsing Works"
---

A user types a line such as `tail -n 5 app.log`, and your program receives clean values. This page explains what happens in between: in which order **ZIO CLI** reads the words, which spellings it accepts, and what it does with words it does not expect. It builds on [Getting Started](getting-started.md). Knowing these rules helps you design commands that read the way users type them, and explains results that look surprising at first, such as an option that is silently ignored.

## The Parse Pipeline

The shell splits the line into words before your program starts. **ZIO CLI** then reads those words from left to right, in a fixed order:

```text
 user types               -n 5 app.log
                               │
 CliApp#run               adds the command name in front
                               │
                               ▼
                          tail -n 5 app.log
                               │
 Command#parse            1. built-in flags? (--help, -h, --wizard, completion)
                               │ no
                          2. command name       tail
                          3. options            -n 5      (stops at the first word that is not an option)
                          4. arguments          app.log
                          5. subcommand         (when the command has subcommands: steps 2-4 again
                               │                 for the subcommand, on the words that are left)
                               │ parse failed?
                          6. --help, -h, --wizard or -w anywhere on the line? show help or the wizard
                               │
                               ▼
 result                   built-in action  or  value + leftover words
                               │
 CliApp#run               prints help / script, or runs your code with the value (leftover is ignored)
```

Each step is covered in its own section below. The reference pages hold the details of each building block: [Commands](commands.md), [Options](options.md), [Args](args.md), [Built-in commands](built-in-commands.md) and [CliConfig](cli-config.md).

## Trying It Yourself

`Command#parse` runs steps 1 to 6 without starting an app, so we can call it directly to see what **ZIO CLI** makes of a line. It takes the words and a `CliConfig`, and returns a ZIO effect. We wrap it in a small helper that runs the effect and prints the outcome on one line:

```scala mdoc:silent
import zio.{Runtime, Unsafe}
import zio.cli._

def show[A](command: Command[A], line: String, config: CliConfig = CliConfig.default): Unit = {
  val words  = line.split(" ").toList
  val result = Unsafe.unsafe { implicit unsafe =>
    Runtime.default.unsafe.run(command.parse(words, config).either).getOrThrowFiberFailure()
  }
  val outcome = result match {
    case Right(CommandDirective.UserDefined(leftover, value)) =>
      s"value = $value, leftover = $leftover"
    case Right(CommandDirective.BuiltIn(BuiltInOption.ShowHelp(synopsis, _))) =>
      s"built-in: help for ${synopsis.helpDoc.toPlaintext(color = false).trim}"
    case Right(CommandDirective.BuiltIn(BuiltInOption.ShowWizard(_))) =>
      "built-in: wizard"
    case Right(CommandDirective.BuiltIn(other)) =>
      s"built-in: $other"
    case Left(error) =>
      s"error: ${error.error.toPlaintext(color = false).trim}"
  }
  println(s"$line  =>  $outcome")
}
```

A successful parse gives a `CommandDirective.UserDefined` with the parsed value and the leftover words that nothing consumed. A built-in flag gives a `CommandDirective.BuiltIn`, and a failure gives a `ValidationError` whose `error` field holds the message the user sees. See [Validation and errors](validation-and-errors.md) for the error types.

Most examples use a small `tail` command. It has an integer option `--lines` (short name `-n`, default 10), a flag `--follow` (short name `-f`) and one text argument:

```scala mdoc:silent
val tail: Command[((BigInt, Boolean), String)] =
  Command(
    "tail",
    Options.integer("lines").alias("n").withDefault(BigInt(10)) ++ Options.boolean("follow").alias("f"),
    Args.text("file")
  )
```

The value is a tuple: first the values of the options, then the value of the argument.

## The Command Name Comes First

The first word must be the name of the command. `Command#parse` expects that word, so the lines in our examples start with `tail`:

```scala mdoc
show(tail, "tail -n 5 app.log")
show(tail, "head -n 5 app.log")
```

A user of your program does not type the command name. When the program is started as `tail -n 5 app.log`, the operating system passes only `-n 5 app.log`, and `CliApp#run` adds the name in front before it calls `Command#parse`. For a command with subcommands, it adds the name of the parent command, so the user starts with the subcommand name. The one exception is a top-level command built with `|`, such as `Command("start", ...) | Command("stop", ...)`: `CliApp#run` adds nothing, and the user types `start` or `stop` as the first word. See [CliApp](cli-app.md) for how `CliApp#run` is called.

Command and subcommand names are matched without regard to case by default, so `TAIL` and `Tail` also select the `tail` command. The [Case Sensitivity](#case-sensitivity) section covers the details.

## Options Come Before Arguments

After the command name, **ZIO CLI** reads options. It takes the next word and compares it with the names of the command's options. If it matches, it reads the option (and its value, if the option takes one) and moves on to the next word. As soon as a word does not match any option, option reading stops, and that word and everything after it are positional arguments.

Options can be given in any order among themselves:

```scala mdoc
show(tail, "tail -n 5 -f app.log")
show(tail, "tail -f -n 5 app.log")
```

Options written after an argument are not read as options. They become leftover words, so an option that has a default silently keeps it:

```scala mdoc
show(tail, "tail app.log -n 5")
```

The output shows `lines` still at 10, and `-n 5` in the leftover list. If the option has no default, the same mistake is an error, because the option is never found. The `greet` command below has a required `--greeting` option:

```scala mdoc:silent
val greet: Command[(String, String)] =
  Command("greet", Options.text("greeting"), Args.text("name"))
```

When `--greeting` comes after the name, parsing fails:

```scala mdoc
show(greet, "greet --greeting Hello Ada")
show(greet, "greet Ada --greeting Hello")
```

Two more consequences of the same rule are worth knowing. A word that looks like an option but matches none, such as `--nope`, also ends option reading, so it becomes a positional argument. And each option is read at most once: a second `--lines` is not a known option any more, so it ends option reading too:

```scala mdoc
show(tail, "tail --nope app.log")
show(tail, "tail --lines 5 --lines 6 app.log")
```

:::caution
Options are not position-independent. Users who are used to tools that accept options anywhere on the line can write `tail app.log -n 5` and get no error, only the default value. Put the options first in your own help text and examples, so users copy that order.
:::

## Ending Options With `--`

The word `--` ends option reading on purpose. Every word after it is a positional argument, even when it starts with `-`. This is how a user passes a value such as `-n` as an argument. The `echo` command below has a flag `-n` and takes any number of words:

```scala mdoc:silent
val echo: Command[(Boolean, List[String])] =
  Command("echo", Options.boolean("n"), Args.text("word").*)
```

Without `--`, a leading `-n` is the flag. After `--`, it is one of the words:

```scala mdoc
show(echo, "echo -n hello")
show(echo, "echo -- -n hello")
show(echo, "echo -n -- -n")
```

:::caution
In a command with subcommands, the parent command reads the first `--` and removes it, so the subcommand never sees it. If the subcommand has options, a word such as `-f` after that `--` is read as one of the subcommand's options again. To pass such a word to a subcommand's argument, write `--` twice: `git add -- -- -f`.
:::

## How to Write Options

This section covers the spellings **ZIO CLI** accepts for an option on the command line. The [Options](options.md) reference page explains how to define each kind of option.

### Long and Short Names

A name with more than one letter is written with two dashes (`--lines`), and a one-letter name with one dash (`-n`). `Options#alias` adds more names to the same option. Every name of an option is equivalent:

```scala mdoc
show(tail, "tail --lines 5 app.log")
show(tail, "tail -n 5 app.log")
```

### Values After `=`

A long name can carry its value after an equals sign. This form only works with two dashes. A short name always takes its value as the next word, and a value glued to it (`-n5`) is not recognised:

```scala mdoc
show(tail, "tail --lines=5 app.log")
show(tail, "tail -n5 app.log")
```

Notice that `-n5` does not match `-n`, so option reading stops and `-n5` becomes the file name.

### Clustered Short Flags

Several one-letter flags can be joined into one word: `-la` means `-l -a`. Clustering works for flags only, meaning boolean options that take no value. The `ls` command below has two flags:

```scala mdoc:silent
val ls: Command[(Boolean, Boolean)] =
  Command("ls", Options.boolean("long").alias("l") ++ Options.boolean("all").alias("a"))
```

Both orders of the cluster set both flags. A cluster that contains an option with a value, such as `-fn 5` for `tail`, is not split, and the whole word is taken as an argument:

```scala mdoc
show(ls, "ls -la")
show(ls, "ls -al")
show(tail, "tail -fn 5 app.log")
```

### Boolean Flags

A boolean option is a flag: writing its name is enough, and it takes no value. `Options.boolean` produces `true` when the flag is present and `false` when it is absent, unless you choose other values. The flag can also be followed by an explicit `true` or `false`, either as the next word or after `=`:

```scala mdoc
show(tail, "tail -f app.log")
show(tail, "tail --follow false app.log")
show(tail, "tail --follow=false app.log")
```

Only the exact words `true` and `false` are read as the flag's value. Any other word after a flag, such as `yes` or `0`, is not consumed by the flag, so it becomes the next positional argument:

```scala mdoc
show(tail, "tail --follow yes app.log")
```

Here `yes` became the file name, and `app.log` is leftover.

## Alternatives With `|`

The `|` operator offers a choice, both between options and between commands.

Between two options, `a | b` accepts exactly one of them. The `load` command reads its input either from a file or from a URL:

```scala mdoc:silent
val load: Command[String] =
  Command("load", Options.text("file") | Options.text("url"))
```

Either option alone works. Giving both is an error, and so is giving neither:

```scala mdoc
show(load, "load --file data.csv")
show(load, "load --url https://example.com/data.csv")
show(load, "load --file data.csv --url https://example.com/data.csv")
show(load, "load")
```

:::caution
Do not put an option that already has a default inside `|`. That includes every `Options.boolean` and every option built with `Options#optional` or `Options#withDefault`. The side with the default always counts as given, so the alternative reports "Options collision detected" for every input, even an empty line. For mutually exclusive modes, use `Options.enumeration`. To make the whole choice optional, add the default to the alternative: `(a | b).withDefault(x)`.
:::

Between two commands, `a | b` tries the first command, and tries the second one only when the first word is not the first command's name:

```scala mdoc:silent
val service: Command[String] =
  Command("start", Args.text("service")) | Command("stop", Args.text("service"))
```

The first word picks the command:

```scala mdoc
show(service, "start web")
show(service, "stop web")
show(service, "restart web")
```

## Subcommands

A command with subcommands, such as `git` with `add` and `clone`, repeats the pipeline. The parent command reads its name and its own options. The first word that is not one of its options must be the name of a subcommand, and that subcommand then reads its own options and arguments from the rest of the line:

```scala mdoc:silent
val git =
  Command("git", Options.boolean("verbose")).subcommands(
    Command("add", Options.boolean("force").alias("f"), Args.text("path")),
    Command("clone", Options.integer("depth").optional, Args.text("url"))
  )
```

The parent's options go before the subcommand name, and the subcommand's options go after it, before the subcommand's arguments:

```scala mdoc
show(git, "git --verbose add notes.txt")
show(git, "git clone --depth 1 https://example.com/repo.git")
```

An option in the wrong place follows the same rules as before. The parent's `--verbose` written after `add` is not an option of `add`, so it becomes the path. The subcommand's `--depth` written after the URL is leftover:

```scala mdoc
show(git, "git add --verbose notes.txt")
show(git, "git clone https://example.com/repo.git --depth 1")
```

A word that is not a known subcommand is an error that lists the valid names, and a line with no subcommand at all shows the help of the parent command:

```scala mdoc
show(git, "git push")
show(git, "git")
```

See [Commands](commands.md) for building subcommands and for the type of the value they produce.

## Auto-Correct: "Did You Mean"

When a word looks like a misspelt option name, **ZIO CLI** can suggest the right one. A word is a candidate when its edit distance to the option's main name (such as `--greeting`) is at most `CliConfig#autoCorrectLimit`, which is 2 by default. The option's name must also be longer than the limit plus one character, so with the default limit, names of three letters or fewer never get a suggestion.

The suggestion is only shown when the whole line fails to parse. With `greet`, the misspelt `--greting` leaves the required `--greeting` missing, so the parse fails and the error carries the suggestion:

```scala mdoc
show(greet, "greet --greting Hello Ada")
```

When the line can still be parsed without the option, there is no error and so no suggestion. The misspelt word ends option reading like any other unknown word, and becomes an argument or a leftover word. The `greetWithDefault` command gives `--greeting` a default:

```scala mdoc:silent
val greetWithDefault: Command[(String, String)] =
  Command("greet", Options.text("greeting").withDefault("Hello"), Args.text("name"))
```

The same typo now runs the command with `--greting` as the name:

```scala mdoc
show(greetWithDefault, "greet --greting Hi Ada")
```

**ZIO CLI** only suggests. It never replaces the word with the corrected name. Set `CliConfig#autoCorrectLimit` in [CliConfig](cli-config.md) to change the threshold, and see [Validation and errors](validation-and-errors.md) for the `CorrectedFlag` error type.

## Case Sensitivity

`CliConfig.default` is case-insensitive (`caseSensitive = false`). This applies to command and subcommand names, so `TAIL` selects `tail`. With `CliConfig(caseSensitive = true, autoCorrectLimit = 2)`, the name must match exactly:

```scala mdoc
show(tail, "TAIL -n 5 app.log")
show(tail, "TAIL -n 5 app.log", CliConfig(caseSensitive = true, autoCorrectLimit = 2))
```

:::caution
The case-insensitive setting does not carry over to option names. An option name in a different case, such as `--LINES`, is not found, even with `CliConfig.default`: an option with a default silently keeps it, and a required option is reported as missing. Users must type option names in the case you declared them in. The values of `Options.enumeration` and `Args.enumeration` are also always compared with their exact case.
:::

To see the effect on a required option:

```scala mdoc
show(greet, "greet --GREETING Hello Ada")
```

## Built-In Flags

Every command understands a few flags that you do not declare: `--help` (or `-h`), `--wizard`, and the shell completion flags `--shell-completion-script` and `--shell-completion-index`, both used together with `--shell-type`. [Built-in commands](built-in-commands.md) describes what each one does.

A built-in flag is recognised in two situations. The first is when the words after the command name are only built-in flags:

```scala mdoc
show(greet, "greet --help")
show(greet, "greet --shell-completion-script /usr/local/bin/greet --shell-type bash")
```

When the line holds anything else, **ZIO CLI** parses it as a normal command line, and a successful parse runs the command. The built-in flag is then just another word. The `echo` command takes any words, so `--help` after `hello` is one of them:

```scala mdoc
show(echo, "echo hello --help")
```

The second situation is a fallback. When the line fails to parse and contains `--help`, `-h`, `--wizard` or `-w` anywhere, **ZIO CLI** shows the help or starts the [wizard](wizard.md) instead of reporting the error. This fallback is controlled by `CliConfig#finalCheckBuiltIn`, which is `true` by default. The `greet` line below fails because `--greeting` is missing, so the user gets help:

```scala mdoc
show(greet, "greet Ada --help")
```

The short form `-w` only works through this fallback. Both completion flags need `--shell-type` next to them. Without it, the line is parsed as a normal command line:

```scala mdoc
show(greet, "greet --shell-completion-script /usr/local/bin/greet")
```

When several built-in flags are given, one wins, in this order: help, then the wizard, then the completion script, then completions:

```scala mdoc
show(greet, "greet --wizard --help")
```

For a subcommand, `--help` after the subcommand name shows the help of that subcommand, with the parent's name in its usage line:

```scala mdoc
show(git, "git add --help")
```

:::caution
`--help` after the name of any subcommand other than the first one in the `subcommands` list shows the help of the first subcommand. In the `git` example, `git clone --help` shows the help of `git add`. The help of the parent command (`git --help`) lists every subcommand with its usage line and description.
:::

## Leftover Words

A successful parse can leave words that nothing consumed: options after the arguments, an extra argument, or a word after a boolean flag. `Command#parse` returns them as `leftover`:

```scala mdoc
show(greet, "greet --greeting Hello Ada Bob")
```

`CliApp#run` ignores the leftover words. It runs your code with the parsed value and reports no error, so the line above greets Ada and drops `Bob` without a word. If your program must reject extra input, give the command an argument that collects the rest of the line, such as `Args.text("rest").*`, and check it in your own code.

## Where to Go Next

- [Tutorial: Build a Git-Like CLI](examples.md) uses these rules in a complete program with subcommands, options with values, help and tests.
- [Options](options.md) and [Args](args.md) list every option and argument type and the values each one accepts.
- [Commands](commands.md) covers `|`, subcommands and mapping the parsed value.
- [Built-in commands](built-in-commands.md) and [CliConfig](cli-config.md) describe the built-in flags and the settings used on this page.
- [Validation and errors](validation-and-errors.md) lists the error types a failed parse produces.
