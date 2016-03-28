package safesql

import scala.concurrent.{ExecutionContext, Future, blocking}
import anorm._
import play.api.libs.concurrent.Akka
import play.api.db.DB
import play.api.Play.current

/**
  * Created by junyan on 3/27/16.
  */
trait MySQLClientComponent {

  trait MySQLClient {
    def executeQuery[T](sql: SimpleSql[Row], rowParser: RowParser[T]): Future[List[T]]

    def executeInsert(sql: SimpleSql[Row]): Future[Option[Long]]

    def executeInsert[T](sql: SimpleSql[Row], keyParser: RowParser[T]): Future[Option[T]]

    def executeUpdate(sql: SimpleSql[Row]): Future[Int]

    def executeBatchInsert(sql: BatchSql): Future[List[Long]]

    def executeBatchInsert[T](sql: BatchSql, keyParser: RowParser[T]): Future[List[T]]

    def executeBatchUpdate(sql: BatchSql): Future[List[Int]]
  }

  val mySQLClient: MySQLClient
}


trait DefaultMySQLClientComponent extends MySQLClientComponent {

  val mysqlContext: ExecutionContext = Akka.system.dispatchers.lookup("contexts.mysql-context")

  override val mySQLClient = new MySQLClient {
    override def executeQuery[T](sql: SimpleSql[Row], rowParser: RowParser[T]): Future[List[T]] = Future(blocking({
      DB.withConnection { implicit c =>
        sql.as(rowParser.*)
      }
    }))(mysqlContext)

    override def executeInsert(sql: SimpleSql[Row]): Future[Option[Long]] = Future(blocking({
      DB.withConnection { implicit c =>
        sql.executeInsert[Option[Long]]()
      }
    }))(mysqlContext)


    override def executeInsert[T](sql: SimpleSql[Row], keyParser: RowParser[T]): Future[Option[T]] =
      Future(blocking({
        DB.withConnection { implicit c =>
          sql.executeInsert(keyParser.singleOpt)
        }
      }))(mysqlContext)

    override def executeUpdate(sql: SimpleSql[Row]): Future[Int] = Future(blocking({
      DB.withConnection { implicit c =>
        sql.executeUpdate()
      }
    }))(mysqlContext)

    override def executeBatchInsert(sql: BatchSql): Future[List[Long]] = Future(blocking({
      DB.withConnection { implicit c =>
        val filledStatement = sql.getFilledStatement(c, getGeneratedKeys = true)
        val updated = filledStatement.executeBatch()
        val rs = filledStatement.getGeneratedKeys
        SqlQueryResult(resultSet = resource.managed(rs), resultSetOnFirstRow = false).as[List[Long]](SqlParser.scalar[Long].*)
      }
    }))(mysqlContext)

    override def executeBatchInsert[T](sql: BatchSql, keyParser: RowParser[T]): Future[List[T]] = Future(blocking({
      DB.withConnection { implicit c =>
        val filledStatement = sql.getFilledStatement(c, getGeneratedKeys = true)
        val updated = filledStatement.executeBatch()
        val rs = filledStatement.getGeneratedKeys
        SqlQueryResult(resultSet = resource.managed(rs), resultSetOnFirstRow = false).as[List[T]](keyParser.*)
      }
    }))(mysqlContext)

    override def executeBatchUpdate(sql: BatchSql): Future[List[Int]] = Future(blocking({
      DB.withConnection { implicit c =>
        sql.execute().toList
      }
    }))(mysqlContext)
  }

}
