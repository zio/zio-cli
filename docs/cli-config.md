---
id: cli-config
title: "Cli Configuration"
---
It is possible to tweak the behaviour of our `CliApp` specifying a custom `CliConfig` in the parameter `cliConfig` of method `CliApp.make`. By default, `CliApp.make` uses `CliConfig.default`. `CliConfig` specify how a `CliApp`
determines the valid commands from the `command: Command[Model]` parameter.

## Parameters

You can construct directly a `CliConfig`:
```scala mdoc:silent
final case class CliConfig(
  caseSensitive: Boolean,
  autoCorrectLimit: Int
)
```

There are two parameters controlled by `CliConfig`.
### Case sensitivity
It is controlled by field `caseSensitive`. If it is `true`, then a `CliApp` will determine as distinct uppercase and lowercase version of a letter in a command. On the other hand, `caseSensitive = false` implies that the `CliApp` will treat uppercase and lowercase letters as the same. In the Git example, we would have:

- `caseSensitive = true`
```
git clone  
GIT cloNE
```
The first will be detected as the `git clone` command while the second will trigger an error.

- `caseSensitive = false`
```
git clone   // Detected by CLI as "git clone" command
GIT cloNE   // Detected by CLI as "git clone" command
```
Both commands will be detected as the `git clone` command.

### Autocorrection
It is controlled by field `autoCorrectLimit`. It is an number that specifies the mistakes that can be corrected when parsing the name of an option introduced by a user. If the CLI detects that the user has written an incorrect name for the option and the number of mistakes is less, it will suggest the correct option. If `autoCorrectLimit=2` and the user inputs 
`git status --bran nameOfBranch` instead of `git status --branch nameOfBranch`, the output produced by the CLI app will be 
```
The flag "--bran" is not recognized. Did you mean --branch?
```
On the other hand, if the user writes `git status --bra nameOfBranch`, the CLI app will not be able to detect the 3 mistakes and will produce
```
Expected to find --branch option.
```


## Default configuration

The default configuration is given by
```scala mdoc:silent
object CliConfig {
  val default: CliConfig = CliConfig(caseSensitive = false, autoCorrectLimit = 2)
}
```
This means that a `CliApp` that does not specify any `CliConfig` and uses `CliConfig.default` will:
- ignore if the letters of a command are written with uppercase or lowercase and
- correct automatically up to two mistakes done when writting the name of an option in a command of `CliApp`.

