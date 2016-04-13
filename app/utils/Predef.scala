package utils

import java.nio.ByteBuffer
import java.text.SimpleDateFormat
import java.util.Date

import org.joda.time.DateTime
import play.api.libs.Codecs
import play.api.mvc.{Result, Results}
import play.mvc.Http.MimeTypes

import scala.util.{Random, Try}

/**
 * @author jun.
 */
object Predef {

  implicit class IterableToRichIterable[T](val iterable: Iterable[T]) {
    def shuffle: Iterable[T] = {
      Random.shuffle(iterable)
    }
  }

  implicit class StringToRichString(val str: String) {
    def base64Decode: Option[Array[Byte]] = {
      Try(java.util.Base64.getDecoder.decode(str)).toOption
    }

    def jodaTimeToMillis: Option[Long] = {
      Try(new DateTime(str).getMillis).toOption
    }

    def strToInteger: Option[Int] = {
      Try(Integer.valueOf(str).toInt).toOption
    }

    def strToLong: Option[Long] = {
      Try(Integer.valueOf(str).toLong).toOption
    }

    def toDate: Option[Date] = {
      Try(new DateTime(str)).toOption.map(_.toDate)
    }
  }

  implicit class LongToRichLong(val value: Long) {
    def toBytes: Array[Byte] = {
      ByteBuffer.allocate(8).putLong(value).array()
    }
  }

  implicit class ByteArrayToRichByteArray(val byteArray: Array[Byte]) {
    def toLong: Option[Long] = {
      val bb = ByteBuffer.wrap(byteArray)
      if (byteArray.length < 8) None else Some(bb.getLong)
    }

    def toHexString: String = {
      Codecs.toHexString(byteArray)
    }
  }

  /**
   * RFC-3339 compliant date formatter.
   * Please refer to https://tools.ietf.org/html/rfc3339#section-5.6
   * @param date The java.util.Date object
   */
  implicit class DateToRichDate(val date: Date) {
    val fullDateFormatter = new SimpleDateFormat("YYYY-MM-dd")

    def toFullDate : String = fullDateFormatter.format(date)
  }

  implicit class TupleToRichTuple[T,V](val tuple: (T,V)) {
    def mapLeft[R](f: T => R): (R, V) = {
      (f(tuple._1), tuple._2)
    }

    def mapRight[R](f: V => R): (T, R) = {
      (tuple._1, f(tuple._2))
    }
  }

  def processResponse(response: Any): Result = {
    Results.Ok(JsonUtils.toJsValue(Map("response" -> response))).as(MimeTypes.JSON)
  }

}
