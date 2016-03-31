package safesql

import anorm._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global


/**
 * @author juyan
 */
abstract class SyntheticSimpleKeyDBTable extends DBTable {

  type ENTITY

  def keyColumn: String

  def mapper: RowParser[ENTITY]

  def requiredFields: Set[String]

  def get(key: Long)(implicit client: MySQLClientComponent): Future[Option[ENTITY]] = {
    val predicate = DBPredicate(keyColumn, (keyColumn, key), DBPredicateRelation.EQUALS)
    val sql = selectStatement(DBPredicates(predicate))
    client.mySQLClient.executeQuery[ENTITY](sql, mapper).map(_.headOption)
  }

  def batchGet(keys: Iterable[Long], predicates: Option[DBPredicates] = None)
    (implicit client: MySQLClientComponent): Future[Iterable[ENTITY]] = {
    if (keys.isEmpty) Future.successful(Seq())
    else {
      val predicate = DBPredicate(keyColumn, (keyColumn, keys.toSeq), DBPredicateRelation.IN_SEQUENCE)
      val sql = selectStatement(DBPredicates(predicate))
      client.mySQLClient.executeQuery[ENTITY](sql, mapper)
    }
  }

  def create(columns: List[(String, ParameterValue)])(implicit client: MySQLClientComponent):
    Future[Option[Long]] = {
    //TODO: Check required fields are covered in columns arg
    val sql = insertStatement(ignore = false, columns)
    client.mySQLClient.executeInsert(sql)
  }

  def batchCreate(columns: List[List[(String, ParameterValue)]])(implicit client: MySQLClientComponent):
    Future[List[Long]] = {
    val sql = batchInsertStatement(ignore = false, columns)
    client.mySQLClient.executeBatchInsert(sql)
  }

  def update(key: Long, columns: List[(String, Either[ParameterValue, NumericOp])])(implicit client: MySQLClientComponent): Future[Boolean] = {
    val keyPredicate = DBPredicate(keyColumn, (keyColumn, key), DBPredicateRelation.EQUALS)
    val sql = updateStatement(columns, DBPredicates(keyPredicate))
    client.mySQLClient.executeUpdate(sql).map(_ == 1)
  }

  def batchUpdate(keys: Seq[Long], columns: Seq[Seq[(String, Either[ParameterValue, NumericOp])]])(implicit client: MySQLClientComponent): Future[Boolean] = {
    if (keys.size != columns.size || keys.isEmpty) throw new IllegalArgumentException("Invalid input for batch update")
    val keysPredicate = keys.map { key =>
      DBPredicates(DBPredicate(keyColumn, (keyColumn, key), DBPredicateRelation.EQUALS))
    }
    val sql = batchUpdateStatement(columns, keysPredicate)
    client.mySQLClient.executeBatchUpdate(sql).map(_.size == keys.size)
  }
}
