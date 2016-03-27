package controllers

import play.api.mvc._
import utils.SPBActions
import utils.Predef._

import scala.concurrent.Future

/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
trait HomeController extends Controller {

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
    Future.successful(processResponse(Map("data" -> "ok")))
  }

}

object HomeController extends HomeController


