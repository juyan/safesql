package utils

import play.api.libs.json._

/**
 * @author jun.
 */
object JsonUtils {

  def toJsValue(obj: Map[String, Any]): JsValue = {
    val resultMap = obj.map { element =>
      val key = element._1
      val value = element._2
      value match {
        case i: Int => (key, JsNumber(i))
        case l: Long => (key, JsNumber(l))
        case b: Boolean => (key, JsBoolean(b))
        case str: String => (key, JsString(str))
        case s: Seq[Any] => (key, toJsValue(s))
        case o: Map[String, Any] => (key, toJsValue(o))
        case j: JsValue => (key, j)
        case unknown => throw new RuntimeException(unknown.toString)
      }
    }.toSeq
    JsObject(resultMap)
  }

  private def toJsValue(seq: Seq[Any]): JsArray = {
    val resultSeq = seq.map {
        case i: Int => JsNumber(i)
        case l: Long => JsNumber(l)
        case b: Boolean => JsBoolean(b)
        case str: String => JsString(str)
        case s: Seq[Any] => toJsValue(s)
        case o: Map[String, Any] => toJsValue(o)
        case j: JsValue => j
        case unknown => throw new RuntimeException(unknown.toString)
    }
    JsArray(resultSeq)
  }
}
