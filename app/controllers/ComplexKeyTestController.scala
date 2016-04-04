package controllers

import cake.GlobalCake
import models.ComplexSynResourceTable
import play.api.libs.json.JsValue
import play.api.mvc.Controller
import safesql.{MySQLClientComponent, TwoKeyPartsComplexKey}
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

  def update(parentId: Long, id: Long) = SPBActions(parse.json).async { request =>
    val updatedData = (request.body \ "data").as[String]
    val params = List(
      (ComplexSynResourceTable.TABLEDATA_FIELD, Left(updatedData.toParameterValue))
    )

    ComplexSynResourceTable.update(TwoKeyPartsComplexKey(parentId.toDBParameter, id.toDBParameter), params)(this)
        .map { updated =>
      processResponse(Map("updated" -> updated))
    }
  }

  def batchUpdate = SPBActions(parse.json).async { request =>
    val dataList = (request.body \ "data").as[List[JsValue]].map { element =>
      val parentId = (element \ "parentId").as[Long]
      val id = (element \ "id").as[Long]
      val updatedData = (element \ "data").as[String]

      (parentId, id, updatedData)
    }

    val params = dataList.map { case (parentId, id, updatedData) =>
      List(
      (ComplexSynResourceTable.TABLEDATA_FIELD, Left(updatedData.toParameterValue))
      )
    }

    val keys = dataList.map { case (parentId, id, updatedData) =>
      TwoKeyPartsComplexKey[Long, Long](parentId.toDBParameter, id.toDBParameter)
    }

    ComplexSynResourceTable.batchUpdate(keys, params)(this).map { updated =>
      processResponse(Map("updated" -> updated))
    }
  }
}

object ComplexKeyTestController extends ComplexKeyTestController with GlobalCake
