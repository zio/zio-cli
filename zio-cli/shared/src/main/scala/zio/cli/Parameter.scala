package zio.cli

trait Parameter

trait Sub extends Parameter {
    def getSubparameters: Map[String, Parameter]
}

trait Validable extends Parameter {
    def isValid(input: String): Boolean
}


