package safesql

import anorm.{ParameterValue, RowParser}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Created by junyan on 3/31/16.
  */
abstract class ComplexKeyDBTable extends DBTable {

  type KEY <: ComplexKey

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

  def batchCreate(columns: List[List[(String, DBParameter)]])(implicit client: MySQLClientComponent):
  Future[List[KEY]] = {
    val parameterValues = columns.map { param =>
      param.map { case (columnName, dbParam) =>
        (columnName, dbParam.DBValue)
      }
    }
    val sql = batchInsertStatement(ignore = false, parameterValues)
    client.mySQLClient.executeBatchInsert(sql).map { insertedIds =>
      val zippedIds = insertedIds.zip(columns)
      zippedIds.map { case (insertedId, list) =>
        keyMapper(insertedId, list)
      }
    }
  }

  def update(key: KEY, columns: List[(String, Either[ParameterValue, NumericOp])])
            (implicit client: MySQLClientComponent): Future[Boolean] = {
    val keyPredicate = key.toPredicates(keyColumns)
    val sql = updateStatement(columns, DBPredicates(keyPredicate, None, DBPredicatesRelation.AND))
    client.mySQLClient.executeUpdate(sql).map(_ == 1)
  }

  def batchUpdate(keys: Seq[KEY], columns: Seq[Seq[(String, Either[ParameterValue, NumericOp])]])
                 (implicit client: MySQLClientComponent) : Future[Boolean] = {
    if (keys.size != columns.size || keys.isEmpty) throw new IllegalArgumentException("Invalid input for batch update")
    val keysPredicate = keys.map { key =>
      key.toPredicates(keyColumns)
    }
    val sql = batchUpdateStatement(columns, keysPredicate)
    client.mySQLClient.executeBatchUpdate(sql).map(_.size == keys.size)
  }
}
