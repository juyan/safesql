package safesql.tables

import anorm._
import play.api.Logger
import safesql._


/**
 * @author juyan
 */
abstract class DBTable {

  def tableName: String

  val logger = Logger("DBTable")

  def columnSet(columns: List[String]) : String = {
    val separated = columns.mkString(",")
    s"($separated)"
  }

  def valueSet(values: List[String]) : String = {
    val separated = values.map { value =>
      s"{$value}"
    }.mkString(",")
    s"($separated)"
  }

  private def insertSQL(ignore: Boolean, columns: List[(String, String)]): String = {
    val ignoreKeyWord = if (ignore) "IGNORE" else ""
    val columnNames = columnSet(columns.map(_._1))
    val valueNames = valueSet(columns.map(_._2))
    s"INSERT $ignoreKeyWord INTO $tableName $columnNames VALUES $valueNames"
  }

  /**
   * Construct a INSERT SQL query
 *
   * @param ignore ignore duplicate key
   * @param columns A list of columns to insert
   * @return A sql query.
   */
  def insertStatement(ignore: Boolean, columns: List[(String, ParameterValue)]): SimpleSql[Row] = {
    if (columns.isEmpty) throw new IllegalArgumentException("Empty values while building insert SQL")
    val params = columns.map { pair =>
      NamedParameter(pair._1, pair._2)
    }
    val columnVariables = columns.map { col =>
      (col._1, col._1)
    }
    SQL(insertSQL(ignore, columnVariables)).on(params: _*)
  }

  /**
   * Construct a batch INSERT SQL query
 *
   * @param ignore ignore duplicate key
   * @param columns A list of parameters for each row to insert.
   * @return A BatchSql object.
   */
  def batchInsertStatement(ignore: Boolean, columns: List[List[(String, ParameterValue)]]): BatchSql = {
    if (columns.isEmpty) throw new IllegalArgumentException("Empty values while building insert SQL")
    val columnVariables = columns.head.map { pair =>
      (pair._1, pair._1)
    }
    val params = columns.map { column =>
      column.map { p =>
        NamedParameter(p._1, p._2)
      }
    }
    BatchSql(insertSQL(ignore, columnVariables), params)
  }

  /**
   * Construct a SELECT SQL query
 *
   * @param predicates A DBPredicates object containing the predicates for the query.
   * @param pagingContext The paging context. Default is start = 0, count = 20.
   * @return A sql query
   */
  def selectStatement(predicates: DBPredicates, pagingContext: PagingContext = PagingContext()): SimpleSql[Row] = {
    val (statement, params) = predicates.toStatement(SequenceGenerator[String]())
    val pagination = pagingContext.toSql
    SQL(s"SELECT * FROM $tableName WHERE $statement $pagination").on(params: _*)
  }

  /**
    * Construct a SELECT SQL query with projection
 *
    * @param projection A DBProjection object specifying the projection.
    * @param predicates A DBPredicates object containing the predicates for the query.
    * @param pagingContext The paging context. Default is start = 0, count = 20.
    * @tparam T The projected type.
    * @return A sql query.
    */
  def selectStatementWithProjection[T](projection: DBProjection[T], predicates: DBPredicates,
                         pagingContext: PagingContext = PagingContext()): SimpleSql[Row] = {
    val (statement, params) = predicates.toStatement(SequenceGenerator[String]())
    val projectedFields = projection.commaSeparatedFields
    val pagination = pagingContext.toSql
    SQL(s"SELECT $projectedFields FROM $tableName WHERE $statement $pagination").on(params: _*)
  }

  private def updateValueToNamedParameter(variableName: String, updateType: Either[ParameterValue, NumericOp]): NamedParameter = {
    val value = updateType.right.map(_.value).merge
    NamedParameter(variableName, value)
  }

  private def transformUpdateData(data: Seq[(String, Either[ParameterValue, NumericOp])],
                                  sequenceGenerator: SequenceGenerator[String]) : (String, Seq[NamedParameter]) = {
    val updateData = data.map {
      case (columnName, updateType) =>
        val variableName = sequenceGenerator.getNameAndSequenceForKey(columnName)
        val namedParam = updateValueToNamedParameter(variableName, updateType)
        updateType match {
          case Left(parameterValue) => (s"$columnName = {$variableName}", namedParam)
          case Right(op) => (s"$columnName = $columnName + {$variableName}", namedParam)
        }
    }

    val updateStatement = updateData.map(_._1).mkString(",")
    val updateParams = updateData.map(_._2)
    (updateStatement, updateParams)
  }

  /**
   * Construct an UPDATE SQL query
 *
   * @param columns columns and values to set or numerically increment/decrement
   * @param predicates predicate on what rows to update
   * @return A SQL query
   */
  def updateStatement(columns: Seq[(String, Either[ParameterValue, NumericOp])], predicates: DBPredicates): SimpleSql[Row] = {
    if (columns.isEmpty) throw new IllegalArgumentException("Empty values while building insert SQL")
    val sequenceGenerator = SequenceGenerator[String]()
    val (whereStatement, whereParams) = predicates.toStatement(sequenceGenerator)
    val (updateStatement, updateParams) = transformUpdateData(columns, sequenceGenerator)
    val allParams = whereParams ++ updateParams
    SQL(s"UPDATE $tableName SET $updateStatement WHERE $whereStatement").on(allParams: _*)
  }

  /**
   * Construct an batched UPDATE SQL query
 *
   * @param columns A list of elements, each stands for the update values for single row
   * @param predicates predicate on what rows to update
   * @return A BatchSql object
   */
  def batchUpdateStatement(columns: Seq[Seq[(String, Either[ParameterValue, NumericOp])]], predicates: Seq[DBPredicates]): BatchSql = {
    if (columns.isEmpty || (predicates.size != columns.size))
      throw new IllegalArgumentException("Empty values while building batch update SQL")
    val predicatesData = predicates.map { predicate =>
      predicate.toStatement(SequenceGenerator[String]())
    }
    val whereStatement = predicatesData.head._1
    val whereParams = predicatesData.map(_._2)

    val updateData = columns.map { column =>
      val generator = SequenceGenerator[String]()
      transformUpdateData(column, generator)
    }

    val updateStatement = updateData.head._1
    val updateParams = updateData.map(_._2)

    val params = whereParams.zip(updateParams).map { case (seq1, seq2) =>
      seq1 ++ seq2
    }
    logger.error(s"$params")
    BatchSql(s"UPDATE $tableName SET $updateStatement WHERE $whereStatement", params)
  }
}
