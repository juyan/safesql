package controllers

import anorm.ParameterValue
import play.api.mvc._
import utils.SPBActions
import utils.Predef._
import cake.GlobalCake
import models.SimpleSynResourceTable
import play.api.libs.json.Json
import safesql.{MySQLClientComponent, NumericOp, NumericOpsType}

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
    SimpleSynResourceTable.get(id)(this).map { entity =>
      processResponse(Map("data" -> entity.flatMap(_.data).getOrElse("nothing")))
    }
  }

  def batchQuery(ids: List[Long]) = SPBActions.async { request =>
    SimpleSynResourceTable.batchGet(ids)(this).map { entities =>
      processResponse(Map("data" -> entities.map(Json.toJson(_)).toList))
    }
  }

  def insert = SPBActions(parse.json).async { request =>
    val data = (request.body \ "data").as[String]
    val createdAt = System.currentTimeMillis()
    val updatedAt = createdAt

    val params = List(
      (SimpleSynResourceTable.TABLEDATA_FIELD, ParameterValue.toParameterValue(data)),
      (SimpleSynResourceTable.CREATEDAT_FIELD, ParameterValue.toParameterValue(createdAt)),
      (SimpleSynResourceTable.UPDATEDAT_FIELD, ParameterValue.toParameterValue(updatedAt))
    )

    SimpleSynResourceTable.create(params)(this).map { id =>
      processResponse(Map("id" -> id.getOrElse(-1)))
    }
  }

  def batchInsert = SPBActions(parse.json).async { request =>
    val data = (request.body \ "data").as[List[String]]

    val params = data.map { d =>
      val createdAt = System.currentTimeMillis()
      List(
        (SimpleSynResourceTable.TABLEDATA_FIELD, ParameterValue.toParameterValue(d)),
        (SimpleSynResourceTable.CREATEDAT_FIELD, ParameterValue.toParameterValue(createdAt)),
        (SimpleSynResourceTable.UPDATEDAT_FIELD, ParameterValue.toParameterValue(createdAt))
      )
    }

    SimpleSynResourceTable.batchCreate(params)(this).map { ids =>
      processResponse(Map("ids" -> ids))
    }
  }

  def update(id: Long) = SPBActions(parse.json).async { request =>
    val updatedData = (request.body \ "data").as[String]
    val updateValue = ParameterValue.toParameterValue(updatedData)
    SimpleSynResourceTable.update(id,
      List((SimpleSynResourceTable.TABLEDATA_FIELD, Left(updateValue))))(this).map { updated =>
      processResponse(Map("updated" -> updated))
    }
  }

  def increment(id: Long) = SPBActions.async { request =>
    SimpleSynResourceTable.update(id,
      List((SimpleSynResourceTable.CREATEDAT_FIELD, Right(new NumericOp(ParameterValue.toParameterValue(1), NumericOpsType.PLUS))))
    )(this).map { updated =>
      processResponse(Map("updated" -> updated))
    }
  }

  def batchUpdate(ids: List[Long]) = SPBActions(parse.json).async { request =>
    val updatedData = (request.body \ "data").as[List[String]].map { element =>
      val updateValue = ParameterValue.toParameterValue(element)
      Seq((SimpleSynResourceTable.TABLEDATA_FIELD, Left(updateValue)))
    }
    SimpleSynResourceTable.batchUpdate(ids, updatedData)(this).map { updated =>
      processResponse(Map("updated" -> updated))
    }
  }

}

object HomeController extends HomeController with GlobalCake


