package zio.cli

import zio.IO

private[cli] trait Parameter {
    lazy val shortDesc: HelpDoc = HelpDoc.empty
}

private[cli] trait Sub extends Parameter {

    private[cli] val alternatives: List[Parameter]

    def getSubparameters: Predef.Map[String, (String, Parameter)] = {
      def extract[B <: Parameter](param: B): Predef.Map[String, (String, Parameter)] =
        param match {
          case p: Sub => p.getSubparameters
          case p: Wrap => extract(p.wrapped)
          case p: Named => Predef.Map((p.name, (p.name, p)))
          case p => Predef.Map(("", ("", p)))
        }
      
      alternatives.map(extract).foldLeft(Map.empty[String, (String, Parameter)])(_ ++ _)
    }
}

private[cli] trait Wrap extends Parameter {
    def wrapped: Parameter
}

private[cli] trait Validable extends Parameter {

    val wizardInfo: HelpDoc

    def isValid(input: String, conf: CliConfig): IO[ValidationError, List[String]]
}

private[cli] trait Lista extends Parameter {
    def lista: (String, List[Parameter])
}

private[cli] trait Named extends Parameter {
    val name: String
}


