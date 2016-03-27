package utils

import play.api.mvc.{Result, RequestHeader, Results}
import play.mvc.Http.MimeTypes

/**
 * @author jun.
 */
case class SPBException (
  code: Int = -1,
  message: String,
  cause: Throwable = null,
  parameters: Map[String, Any] = Map.empty[String, Any],
  status: Results.Status = Results.InternalServerError,
  data: Map[String, Any] = Map.empty[String, Any]
) extends RuntimeException(message, cause)

object SPBException {

  def buildResponse(exception: SPBException)(implicit requestHeader: RequestHeader): Result = {
    val errorDataMap = Map(
      "code" -> exception.code,
      "message" -> exception.message,
      "parameters" -> exception.parameters,
      "data" -> exception.data
    )

    val errorMap = Map("error" -> errorDataMap)
    exception.status(JsonUtils.toJsValue(errorMap)).as(MimeTypes.JSON)
  }
}

