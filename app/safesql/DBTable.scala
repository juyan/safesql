package safesql

import anorm._


/**
 * @author juyan
 */
abstract class DBTable {

  def tableName: String

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
    val valueNames = valueSet(columns.map(_._1))
    s"INSERT $ignoreKeyWord INTO $tableName $columnNames VALUES $valueNames"
  }

  /**
   * Construct a INSERT SQL query
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
   * @param predicates A DBPredicates object standing for the predicates for the query.
   * @return A sql query
   */
  def selectStatement(predicates: DBPredicates): SimpleSql[Row] = {
    val statement = predicates.toStatement
    val params = predicates.params
    SQL(s"SELECT * FROM $tableName WHERE $statement").on(params: _*)
  }

  private def updateSQL(columns: Seq[(String, Either[ParameterValue, NumericOp])], predicates: DBPredicates): String = {
    val updateStatement = columns.map { v =>
      val updateType = v._2
      val columnName = v._1
      updateType match {
        case Left(parameterValue) => s"$columnName = {$columnName}"
        case Right(op) => s"$columnName = $columnName + {$columnName}"
      }
    }.mkString(",")
    val whereStatement = predicates.toStatement
    s"UPDATE $tableName SET $updateStatement WHERE $whereStatement"
  }

  private def updateValueToNamedParameter(updateValue: (String, Either[ParameterValue, NumericOp])): NamedParameter = {
    val value = updateValue._2.right.map(_.value).merge
    NamedParameter(updateValue._1, value)
  }

  /**
   * Construct an UPDATE SQL query
   * @param columns columns and values to set or numerically increment/decrement
   * @param predicates predicate on what rows to update
   * @return A SQL query
   */
  def updateStatement(columns: List[(String, Either[ParameterValue, NumericOp])], predicates: DBPredicates): SimpleSql[Row] = {
    if (columns.isEmpty) throw new IllegalArgumentException("Empty values while building insert SQL")
    val params = columns.map(updateValueToNamedParameter) ++ predicates.params
    SQL(updateSQL(columns, predicates)).on(params: _*)
  }

  /**
   * Construct an batched UPDATE SQL query
   * @param columns A list of elements, each stands for the update values for single row
   * @param predicates predicate on what rows to update
   * @return A BatchSql object
   */
  def batchUpdateStatement(columns: Seq[Seq[(String, Either[ParameterValue, NumericOp])]], predicates: Seq[DBPredicates]): BatchSql = {
    if (columns.isEmpty || (predicates.size != columns.size)) throw new IllegalArgumentException("Empty values while building batch update SQL")
    val statement = updateSQL(columns.head, predicates.head)
    val params = columns.map { column =>
      column.map(updateValueToNamedParameter)
    }
    BatchSql(statement, params)
  }
}
