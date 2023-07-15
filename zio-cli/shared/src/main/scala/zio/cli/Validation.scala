package zio.cli

final case class ValidationError(validationErrorType: ValidationErrorType, error: HelpDoc) extends Throwable {
  def isOptionMissing: Boolean = validationErrorType == ValidationErrorType.MissingValue
}

sealed trait ValidationErrorType
object ValidationErrorType {
  case object InvalidValue      extends ValidationErrorType
  case object MissingValue      extends ValidationErrorType
  case object CommandMismatch   extends ValidationErrorType
  case object MissingSubCommand extends ValidationErrorType
  case object InvalidArgument   extends ValidationErrorType
}
