package safesql

/**
  * Created by junyan on 4/6/16.
  */
case class PagingContext(
  start: Int = 0,
  count: Int = 20
)
{
  def toSql: String = {
    s"LIMIT $start,$count"
  }
}
