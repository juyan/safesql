package utils

import anorm.SqlQuery
import anorm.SQL
import play.api.Logger

/**
 * A simple SQL string constructor
 * @author jun.
 */
object SQLConstructor {

  def and(arg1: String, arg2: Option[String]) : String = {
    arg2.map{ arg => s"($arg1 AND $arg)"}.getOrElse(arg1)
  }

  def or(arg1: String, arg2: Option[String]) : String = {
    arg2.map{ arg => s"($arg1 OR $arg)"}.getOrElse(arg1)
  }

  def simpleQuery(select: Iterable[String], from: String, where: String) : SqlQuery = {
    val selectClause = select.mkString("SELECT ", ",", " ")
    val fromClause = s"FROM $from "
    val whereClause = s"WHERE $where"
    Logger.info(selectClause + fromClause + whereClause)
    SQL(selectClause + fromClause + whereClause)
  }

  def insertStatement(insert: Seq[(String, String)], into: String) : SqlQuery = {
    val insertClause = s"INSERT INTO $into ${insert.map(_._1).mkString("(", ",", ")")} "
    val valuesClause = s"VALUES ${insert.map(_._2).map{ alias => s"{$alias}"}.mkString("(", ",", ")")}"
    Logger.info(insertClause + valuesClause)
    SQL(insertClause + valuesClause)
  }
}
