---
id: built-in-commands
title: "Built-in commands"
---
**ZIO CLI** automatically constructs some common commands:
- `--help` or `-h` shows a description of the CLI's commands
- `--wizard` executes wizard mode
- `--shell-completion-index`, `--shell-completion-script` and `--shell-type` are used for Bash and Zsh Completion.

These commands are automatically generated also for all subcommands of a command.
```scala mdoc:silent
// Prints help of the cliApp command
cliApp.run("-h")

// Prints help of subcommand
cliApp.run("subcommand", "-h")

```
