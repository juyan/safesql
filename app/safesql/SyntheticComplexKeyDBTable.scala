package safesql

import scala.concurrent.Future

/**
  * Created by junyan on 3/31/16.
  */
abstract class SyntheticComplexKeyDBTable extends DBTable {

  type KEY

  type ENTITY

  def keyColumns: Seq[String]
}
