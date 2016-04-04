package safesql

import anorm.{ParameterValue, RowParser}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Created by junyan on 3/31/16.
  */
abstract class ComplexKeyDBTable extends DBTable {

  type KEYPART1

  type KEY <: ComplexKey[KEYPART1]

  type ENTITY

  def keyColumns: IndexedSeq[String]

  def mapper: RowParser[ENTITY]

  def keyMapper: (Long, List[(String, DBParameter)]) => KEY

  def requiredFields: Set[String]

  def get(key: KEY)(implicit client: MySQLClientComponent): Future[Iterable[ENTITY]] = {
    val predicates = key.toPredicates(keyColumns)
    val sql = selectStatement(predicates)
    client.mySQLClient.executeQuery[ENTITY](sql, mapper)
  }

  def getWithProjection[T](key: KEY, projection: DBProjection[T])(implicit client: MySQLClientComponent): Future[Iterable[T]] = {
    val predicates = key.toPredicates(keyColumns)
    val sql = selectStatement(projection, predicates)
    client.mySQLClient.executeQuery[T](sql, projection.mapper)
  }

  def create(columns: List[(String, DBParameter)])(implicit client: MySQLClientComponent): Future[Option[KEY]] = {
    //TODO: Check required fields are covered in columns arg
    val parameterValues = columns.map { case (columnName, dbParam) =>
      (columnName, dbParam.DBValue)
    }
    val sql = insertStatement(ignore = false, parameterValues)
    client.mySQLClient.executeInsert(sql).map { maybeId =>
      maybeId.map(keyMapper(_, columns))
    }
  }
}
