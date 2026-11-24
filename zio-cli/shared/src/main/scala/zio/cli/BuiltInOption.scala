package zio.cli

/**
 * An option that ZIO CLI handles itself instead of passing it to the application: help, the wizard mode and shell
 * completion. `Command#parse` returns it in a `CommandDirective.BuiltIn`, and `CliApp#run` carries it out.
 *
 * {{{
 * val ls = Command("ls", Args.text("dir"))
 *
 * // succeeds with CommandDirective.BuiltIn(BuiltInOption.ShowHelp(...))
 * val result = ls.parse(List("ls", "--help"), CliConfig.default)
 * }}}
 *
 * @see
 *   [[https://zio.dev/zio-cli/built-in-commands/ Built-in Commands]]
 */
sealed trait BuiltInOption extends Product with Serializable

/**
 * The cases of [[BuiltInOption]] and the options that select them.
 */
object BuiltInOption extends PathPlatformSpecific {
  // TODO add built-in option to show version
  /**
   * Show the help: the usage `synopsis` and `helpDoc`. Selected by `--help` or `-h`.
   */
  final case class ShowHelp(synopsis: UsageSynopsis, helpDoc: HelpDoc) extends BuiltInOption

  /**
   * Print a shell completion script for `shellType` that calls `pathToExecutable`. Selected by
   * `--shell-completion-script <path>` together with `--shell-type`.
   */
  final case class ShowCompletionScript(pathToExecutable: JPath, shellType: ShellType) extends BuiltInOption

  /**
   * Print the completions of the word at `index` of the command line, which the completion script passes in the
   * `COMP_WORD_` environment variables. Selected by `--shell-type` together with `--shell-completion-index`.
   */
  final case class ShowCompletions(index: Int, shellType: ShellType) extends BuiltInOption

  /**
   * Start the wizard mode for `command`. Selected by `--wizard`.
   */
  final case class ShowWizard(command: Command[_]) extends BuiltInOption

  /**
   * The values of all built-in options as read from the command line, before `builtInOptions` picks one.
   */
  final case class BuiltIn(
    help: Boolean,
    shellCompletionScriptPath: Option[JPath],
    shellType: Option[ShellType],
    shellCompletionIndex: Option[Int],
    wizard: Boolean
  )

  /**
   * Reads `--help` (or `-h`), `--shell-completion-script`, `--shell-type`, `--shell-completion-index` and `--wizard`,
   * and picks one [[BuiltInOption]]: help first, then the wizard, then a completion script (which needs a path and a
   * shell type), then completions (which need a shell type and an index). Produces `None` when none of them applies.
   * `--shell-type` accepts `bash`, `sh` and `zsh`.
   */
  def builtInOptions(
    command: => Command[_],
    usageSynopsis: => UsageSynopsis,
    helpDoc: => HelpDoc
  ): Options[Option[BuiltInOption]] = {
    val options = (
      Options.boolean("help").alias("h") ++
        Options.file("shell-completion-script").optional ++
        ShellType.option.optional ++
        Options.integer("shell-completion-index").map(_.toInt).optional ++
        Options.boolean("wizard")
    ).as(BuiltIn.apply _)

    options.map {
      case BuiltIn(true, _, _, _, _)                      => Some(ShowHelp(usageSynopsis, helpDoc))
      case BuiltIn(_, _, _, _, true)                      => Some(ShowWizard(command))
      case BuiltIn(_, Some(path), Some(shellType), _, _)  => Some(ShowCompletionScript(path, shellType))
      case BuiltIn(_, _, Some(shellType), Some(index), _) => Some(ShowCompletions(index, shellType))
      case _                                              => None
    }
  }
}
