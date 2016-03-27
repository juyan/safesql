package utils

import play.api.mvc.Results

/**
 * @author jun.
 */
object SPBError {
  def requestFieldMissing = SPBException(1, message = "Missing fields", status = Results.BadRequest)
  def unauthorized = SPBException(2, message = "unauthorized", status = Results.Unauthorized)
}
