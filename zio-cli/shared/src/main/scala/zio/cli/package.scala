package zio

package object cli {

  object builder {
    sealed trait CommandInfo
    sealed trait ZeroTimes extends CommandInfo
    sealed trait OneTime   extends CommandInfo

    type IsZeroTimes[T] = T =:= ZeroTimes
    type IsOneTime[T]   = T =:= OneTime

    final case class CommandBuilder[
      WithName <: CommandInfo,
      WithOptions <: CommandInfo,
      WithArgs <: CommandInfo,
      WithHelpDoc <: CommandInfo
    ] protected[builder] (
      name: Option[String],
      options: Option[Options[_]],
      args: Option[Args[_]],
      helpDoc: Option[HelpDoc]
    ) { self =>

      // implicit evidence that the command's type does not already carry Name
      def name[N <: WithName: IsZeroTimes](name: String): CommandBuilder[OneTime, WithOptions, WithArgs, WithHelpDoc] =
        copy[OneTime, WithOptions, WithArgs, WithHelpDoc](name = Some(name))

      def options[O <: WithOptions: IsZeroTimes, OptionsType](
        options: Options[OptionsType]
      ): CommandBuilder[WithName, OneTime, WithArgs, WithHelpDoc] =
        copy[WithName, OneTime, WithArgs, WithHelpDoc](options = Some(options))

      def args[A <: WithArgs: IsZeroTimes, ArgsType](
        args: Args[ArgsType]
      ): CommandBuilder[WithName, WithOptions, OneTime, WithHelpDoc] =
        copy[WithName, WithOptions, OneTime, WithHelpDoc](args = Some(args))

      def helpDoc[H <: WithHelpDoc: IsZeroTimes](
        helpDoc: HelpDoc
      ): CommandBuilder[WithName, WithOptions, WithArgs, OneTime] =
        copy[WithName, WithOptions, WithArgs, OneTime](helpDoc = Some(helpDoc))

      // .get is safe because though I can't prove it to the compiler, the only available apply method provides
      // defaults for args, options, and helpDoc, and N must be OneTime
      def build[N <: WithName: IsOneTime, O <: WithOptions, A <: WithArgs, H <: WithHelpDoc]: Command[(_, _)] =
        Command(
          name = name.get,
          options = options.get,
          args = args.get,
          helpDoc = helpDoc.get
        )

    }

    object CommandBuilder {
      def apply() = new CommandBuilder[ZeroTimes, ZeroTimes, ZeroTimes, ZeroTimes](
        None,
        Some(Options.none),
        Some(Args.none),
        Some(HelpDoc.empty)
      )
    }
  }
}
