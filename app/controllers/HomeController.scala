package controllers

import play.api.mvc._
import utils.SPBActions
import utils.Predef._

import anorm._
import cake.GlobalCake
import safesql.MySQLClientComponent
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
trait HomeController extends Controller {
  self: MySQLClientComponent =>

  /**
   * Create an Action to render an HTML page with a welcome message.
   * The configuration in the `routes` file means that this method
   * will be called when the application receives a `GET` request with
   * a path of `/`.
   */
  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }

  def query(id: Long) = SPBActions.async { request =>
    val sql = SQL("SELECT tableData FROM SIMPLE_SYN WHERE id = {id}").on(NamedParameter("id", id))
    mySQLClient.executeQuery(sql, SqlParser.str("tableData")).map { data =>
      processResponse(Map("data" -> data.headOption.getOrElse("nothing")))
    }
  }

}

object HomeController extends HomeController with GlobalCake


