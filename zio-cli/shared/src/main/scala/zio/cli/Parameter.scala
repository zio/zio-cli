package zio.cli

import zio.IO

/**
 * Abstraction employed by the `Wizard` class. `Command`, `Options` and `Args` all extend `Parameter`, and the wizard handles each sub-trait (`Alternatives`, `Input`, `Pipeline`) in its own way: it asks the user to pick one of several alternatives, reads a value for an input, and walks through the parameters of a pipeline in order.
 */
private[cli] trait Parameter {

  /**
   * A one-line description that the wizard shows for this parameter. It is empty unless a sub-type overrides it: an `Options.Single` named `name` gives `Option "name". ` followed by its description.
   */
  def shortDesc: String = ""

  /** The full help that the wizard shows when the user types `help`. */
  def helpDoc: HelpDoc

  /**
   * The kind of parameter that wizard prompts name: `command`, `option` or `argument`.
   */
  def tag: String

}

/**
 * Alternatives are used to allow the user choosing between possibilities. `Command.OrElse` and `Options.OrElse` extend it, and the wizard asks the user to pick one.
 */
private[cli] trait Alternatives extends Parameter {

  /** The parameters to choose from. */
  private[cli] val alternatives: List[Parameter]

  /**
   * Returns the choices that the wizard lists, keyed by name. Each value is the word to add to the command line and the chosen parameter.
   *
   * Nested alternatives are flattened and wrappers (`Wrap`) are looked through. A named parameter, such as a command, is keyed by its first name; a parameter without a name is keyed by `-` and adds an empty word. A later entry replaces an earlier one with the same key, so the two sides of an `Options.OrElse`, which have no names, give a single entry for the right side.
   */
  def getSubparameters: Map[String, (String, Parameter)] = {
    def extract[B <: Parameter](param: B): Map[String, (String, Parameter)] =
      param match {
        case p: Alternatives => p.getSubparameters
        case p: Wrap         => extract(p.wrapped)
        case p: Named        =>
          p.names.headOption match {
            case Some(value) => Map((value, (value, p)))
            case None        => Map(("-", ("", p)))
          }
        case p => Map(("-", ("", p)))
      }

    alternatives.map(extract).foldLeft(Map.empty[String, (String, Parameter)])(_ ++ _)
  }
}

/**
 * Input is used to obtain a parameter from user: the wizard reads a line for it and checks it with `isValid`. `Args.Single`, `Args.Variadic`, `Options.Single`, `Options.WithDefault` and `Options.KeyValueMap` extend it.
 */
private[cli] trait Input extends Parameter {

  /**
   * Splits `args` into the words that belong to this parameter and the remaining words. `Options.Single` takes its flag and value from the front; `Args.Single` and `Args.Variadic` return two empty lists.
   */
  def parse(args: List[String], conf: CliConfig): IO[ValidationError, (List[String], List[String])]

  /**
   * Checks a line that the user typed in the wizard, and returns the words to add to the command line.
   */
  def isValid(input: String, conf: CliConfig): IO[ValidationError, List[String]]
}

/**
 * Pipeline returns a String to add to parameters and a list of new parameters that must be obtained, in order.
 */
private[cli] trait Pipeline extends Parameter {

  /**
   * Returns the word to add to the command line first and the parameters that the wizard asks for next, in order.
   */
  def pipeline: (String, List[Parameter])
}

/**
 * A parameter that wraps another one, such as the `Map` cases of `Command`, `Options` and `Args`, or `Options.OAuth2Options`. `Alternatives#getSubparameters` looks through it to the wrapped parameter.
 */
private[cli] trait Wrap extends Parameter {

  /** The parameter inside this wrapper. */
  def wrapped: Parameter
}

/**
 * Represent a parameter with name to be used as the options in Alternatives. `Command` extends it.
 */
private[cli] trait Named extends Parameter {

  /** The names of this parameter; `Alternatives#getSubparameters` keys the choice by the first one. */
  def names: Set[String]
}
