package zio.cli

final case class ValidationError(validationErrorType: ValidationErrorType, error: HelpDoc) extends Throwable {
  def isOptionMissing: Boolean = validationErrorType == ValidationErrorType.MissingValue
}

sealed trait ValidationErrorType
object ValidationErrorType {
  // needed because KeyValueMap depends on a Single[String] option
  case class KeyValuesDetected(keyValues: List[String]) extends ValidationErrorType

  case class UnclusteredFlag(unclustered: List[String], tail: List[String]) extends ValidationErrorType
  case object InvalidValue                                                  extends ValidationErrorType
  case object MissingValue                                                  extends ValidationErrorType
  case object MissingFlag                                                   extends ValidationErrorType
  case object CorrectedFlag                                                 extends ValidationErrorType
  case object CommandMismatch                                               extends ValidationErrorType
  case object MissingSubCommand                                             extends ValidationErrorType
  case object InvalidArgument                                               extends ValidationErrorType
}
