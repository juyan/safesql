package controllers

import anorm.ParameterValue
import cake.GlobalCake
import models.ComplexSynResourceTable
import play.api.mvc.Controller
import safesql.MySQLClientComponent
import utils.SPBActions
import utils.Predef._
import scala.concurrent.ExecutionContext.Implicits.global

import safesql.DBParameter._



/**
  * Created by junyan on 4/3/16.
  */
trait ComplexKeyTestController extends Controller {
  self: MySQLClientComponent =>

  def insert = SPBActions(parse.json).async { request =>
    val data = (request.body \ "data").as[String]
    val parentId = (request.body \ "parentId").as[Long]
    val createdAt = System.currentTimeMillis()
    val updatedAt = createdAt

    val params = List(
      (ComplexSynResourceTable.PARENTID_FIELD, parentId.toDBParameter),
      (ComplexSynResourceTable.TABLEDATA_FIELD, data.toDBParameter),
      (ComplexSynResourceTable.CREATEDAT_FIELD, createdAt.toDBParameter),
      (ComplexSynResourceTable.UPDATEDAT_FIELD, updatedAt.toDBParameter)
    )

    ComplexSynResourceTable.create(params)(this).map { key =>
      processResponse(Map("key1" -> key.map(_.firstKeyPart).getOrElse(-1),
        "key2" -> key.flatMap(_.secondKeyPart).getOrElse(-1)))
    }
  }
}

object ComplexKeyTestController extends ComplexKeyTestController with GlobalCake
