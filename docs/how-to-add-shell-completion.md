---
id: how-to-add-shell-completion
title: "How to add shell completion (bash)"
---

Let users press Tab to complete the options and subcommands of your CLI in bash.

Only bash is supported. Zsh is not yet implemented: `--shell-type zsh` fails, because the script generator for zsh is a placeholder (`CompletionScript.scala`, line 8).

You need:
- A CLI built with `CliApp.make`. Every `CliApp` gets the built-in options for completion, so you do not add them yourself.
- The installed executable of your CLI on a stable path, such as `~/.local/bin`.
- bash.

## Steps

1. Define your CLI as usual. Completion uses the same `Command` definition as the rest of the program:

```scala mdoc:compile-only
import zio.Console.printLine
import zio.cli.HelpDoc.Span.text
import zio.cli._

object GreetApp extends ZIOCliDefault {
  val greet: Command[String] = Command("greet", Options.text("name"))

  val cliApp = CliApp.make(
    name = "greet",
    version = "1.0.0",
    summary = text("Greets a user"),
    command = greet
  ) { name =>
    printLine(s"Hello, $name!")
  }
}
```

2. Build and install the executable. The completion script calls the executable at the path you pass in step 3, so pass the path where users will run it.

3. Generate the completion script. Pass the path to the executable and `--shell-type bash`. Both options are required:

```bash
my-cli-app --shell-completion-script $(which my-cli-app) --shell-type bash > completion-script.sh
```

4. Load the script. To test it in the current shell session, run:

```bash
source completion-script.sh
```

   To enable completion for every session, copy the file to a directory that bash loads completion scripts from, such as `/etc/bash_completion.d/`. Ship the generated file with your program and install it during packaging.

5. Test it. Open a bash session, type `my-cli-app ` (with a trailing space), and press Tab. The CLI prints the matching names, one per line, and bash shows them.

Each Tab press runs your executable with `--shell-completion-index` and `--shell-type bash`, and passes the words typed so far in `COMP_WORD_0`, `COMP_WORD_1`, and so on. In this case the program prints completions, not a script. See [Bash completion](bash-and-zsh-completion.md) for the full flow.

## See also

- [Bash completion](bash-and-zsh-completion.md): the reference for the completion flow and the environment variables.
- [Options](options.md): how the options that complete are defined.
- [Commands](commands.md): how the commands and subcommands that complete are defined.
