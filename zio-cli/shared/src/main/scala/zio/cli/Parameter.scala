package zio.cli

import zio.IO

/**
 * Abstraction employed by Wizard class. Parameter trait encompass Command, Options and Args traits. Wizard processes
 * subtraits of Parameter in different manners.
 */
private[cli] trait Parameter {

  def shortDesc: String = ""

  def helpDoc: HelpDoc

  def tag: String

}

/**
 * Alternatives are used to allow the user choosing between possibilities.
 */
private[cli] trait Alternatives extends Parameter {

  private[cli] val alternatives: List[Parameter]

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
 * Input is used to obtain a parameter from user.
 */
private[cli] trait Input extends Parameter {

  def parse(args: List[String], conf: CliConfig): IO[ValidationError, (List[String], List[String])]

  def isValid(input: String, conf: CliConfig): IO[ValidationError, List[String]]
}

/**
 * Pipeline returns a String to add to parameters and a list of new parameters that must be obtained.
 */
private[cli] trait Pipeline extends Parameter {
  def pipeline: (String, List[Parameter])
}

/**
 * Wrap allows to ignore Map classes
 */
private[cli] trait Wrap extends Parameter {
  def wrapped: Parameter
}

/**
 * Represent a parameter with name to be used as the options in Alternatives.
 */
private[cli] trait Named extends Parameter {
  def names: Set[String]
}
