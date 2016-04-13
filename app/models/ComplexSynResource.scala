package models

import anorm.{RowParser, SqlParser, ~}
import play.api.libs.json.Json
import safesql.{DBParameter, TwoKeyPartsComplexKey}
import safesql.DBParameter._
import safesql.tables.ComplexKeyDBTable

/**
  * Created by junyan on 4/2/16.
  */
case class ComplexSynResource(
  parentId: Long,
  id: Long,
  tableData: Option[String],
  createdAt: Option[Long],
  updatedAt: Option[Long]
)

object ComplexSynResource {
  implicit val complexSynResourceReads = Json.format[ComplexSynResource]
}

object ComplexSynResourceTable extends ComplexKeyDBTable {

  type KEY = TwoKeyPartsComplexKey[Long, Long]

  type ENTITY = ComplexSynResource

  override def tableName = "COMPLEX_SYN"

  override def keyColumns = IndexedSeq(PARENTID_FIELD, ID_FIELD)

  override def requiredFields = Set(PARENTID_FIELD)

  override def mapper: RowParser[ComplexSynResource] = {
    val parser = SqlParser.long(PARENTID_FIELD) ~
      SqlParser.long(ID_FIELD) ~
      SqlParser.str(TABLEDATA_FIELD).? ~
      SqlParser.long(CREATEDAT_FIELD).? ~
      SqlParser.long(UPDATEDAT_FIELD).?

    parser.map { case parentId~id~data~createdAt~updatedAt =>
      ComplexSynResource(
        parentId = parentId,
        id = id,
        tableData = data,
        createdAt = createdAt,
        updatedAt = updatedAt
      )
    }
  }

  override def keyMapper: (Long, List[(String, DBParameter)]) => TwoKeyPartsComplexKey[Long, Long] = { (id, list) =>
    val parentId = list.find(_._1 == keyColumns(0)).map(_._2.value.asInstanceOf[Long]).get
    TwoKeyPartsComplexKey[Long, Long](parentId.toDBParameter, id.toDBParameter)
  }

  val PARENTID_FIELD = "parentId"
  val ID_FIELD = "id"
  val TABLEDATA_FIELD = "tableData"
  val CREATEDAT_FIELD = "createdAt"
  val UPDATEDAT_FIELD = "updatedAt"

}

