package zio.cli

final case class ValidationError(validationErrorType: ValidationErrorType, error: HelpDoc) {
  def isOptionMissing: Boolean = validationErrorType == ValidationErrorType.MissingValue
}

sealed trait ValidationErrorType
object ValidationErrorType {
  case object InvalidValue extends ValidationErrorType
  case object MissingValue extends ValidationErrorType
}
