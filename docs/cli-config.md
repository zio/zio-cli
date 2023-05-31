---
id: cli-config
title: "Cli Configuration"
---
It is possible to tweak the behaviour of our `CliApp` specifying a custom `CliConfig` in the parameter `cliConfig` of method `CliApp.make`. By default, `CliApp.make` uses `CliConfig.default`. `CliConfig` specify how a `CliApp`
determines the valid commands from the `command: Command[Model]` parameter.

You can construct directly a `CliConfig`:
```scala mdoc:silent
final case class CliConfig(
  caseSensitive: Boolean,
  autoCorrectLimit: Int
)
```

The default configuration is
```scala mdoc:silent
val default: CliConfig = CliConfig(caseSensitive = false, autoCorrectLimit = 2)
```

