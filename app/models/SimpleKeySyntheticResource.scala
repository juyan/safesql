package models

import anorm.SqlParser
import anorm.{RowParser, ~}
import safesql.SyntheticSimpleKeyDBTable

/**
  * Created by junyan on 3/27/16.
  */
case class SimpleSynResource(
  id: Long,
  data: Option[String],
  createdAt: Option[Long],
  updatedAt: Option[Long]
)

object SimpleSynResourceTable extends SyntheticSimpleKeyDBTable {
  type ENTITY = SimpleSynResource

  override def tableName = "SIMPLE_SYN"

  override def keyColumn = ID_FIELD

  override def requiredFields = Set(TABLEDATA_FIELD, CREATEDAT_FIELD, UPDATEDAT_FIELD)

  override def mapper : RowParser[SimpleSynResource] = {
    val parser = SqlParser.long(ID_FIELD) ~
    SqlParser.str(TABLEDATA_FIELD).? ~
    SqlParser.long(CREATEDAT_FIELD).? ~
    SqlParser.long(UPDATEDAT_FIELD).?

    parser.map { case id~data~createdAt~updatedAt =>
      SimpleSynResource(
        id = id,
        data = data,
        createdAt = createdAt,
        updatedAt = updatedAt
      )
    }
  }

  val ID_FIELD = "id"
  val TABLEDATA_FIELD = "tableData"
  val CREATEDAT_FIELD = "createdAt"
  val UPDATEDAT_FIELD = "updatedAt"
}
