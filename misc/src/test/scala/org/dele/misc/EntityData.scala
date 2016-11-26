package org.dele.misc

import org.json4s.{CustomSerializer, DefaultFormats, Extraction}
import org.json4s.JsonAST.{JField, JObject}
import org.json4s.jackson.JsonMethods._

import scala.collection.mutable.ListBuffer

/**
  * Created by jiaji on 11/26/2016.
  */
object EntityData {

  case class EntDetail(name:String, `type`: String, longname:Option[String], created: Option[String], curated: Int)
  case class EntityDetails(entMap:Map[String,EntDetail])
  case class EntData(next_page_start: Option[String], status: String, entities: Array[String], entity_details: EntityDetails)

  object EntityDetailsSerializer extends CustomSerializer[EntityDetails] (format => (
    {
      case JObject(fieldList) => {
        fieldList match {
          case fields:List[_] => {
            val buf = ListBuffer[(String,EntDetail)]()
            fields.foreach{ f =>
              f match {
                case JField(id, obj) => {
                  try {
                    implicit val _format = DefaultFormats
                    buf += id -> obj.extract[EntDetail]
                  }
                  catch {
                    case x:Throwable => println(x)
                  }
                }
                case _ => throw new IllegalArgumentException("todo")
              }
            }
            EntityDetails(buf.toMap)
          }
          case _ => throw new IllegalArgumentException("todo")
        }
      }
      case _ => throw new IllegalArgumentException("todo")
    },
    {
      case x:EntityDetails => {
        val ordered = x.entMap.toList.sortBy(_._1)
        implicit val _format = DefaultFormats
        JObject(ordered.map(p => JField(p._1, Extraction.decompose(p._2))))
      }
    }
    ))

  import org.json4s._
  object Ser {
    implicit val _format = DefaultFormats + EntityDetailsSerializer

    def p(j:String):EntData = parse(j).extract[EntData]
  }

}
