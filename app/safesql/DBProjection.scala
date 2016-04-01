package safesql

import anorm.RowParser

/**
  * Created by junyan on 3/31/16.
  */
class DBProjection[T](val fields: Seq[String], val mapper: RowParser[T]) {
  def commaSeparatedFields: String = fields.mkString(",")
}

object DBProjection {
  def apply[T](fields: Seq[String], mapper: RowParser[T]) = {
    new DBProjection[T](fields, mapper)
  }
}
