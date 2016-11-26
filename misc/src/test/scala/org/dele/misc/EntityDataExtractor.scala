package org.dele.misc

import java.io.InputStream

import org.apache.commons.io.IOUtils
import org.dele.misc.EntityData.EntDetail


/**
  * Created by jiaji on 11/26/2016.
  */
object EntityDataExtractor extends App {

  import tgz.TgzUtil._
  val defaultEncoding = "UTF-8"

  def extractOne(in:InputStream):Map[String,EntDetail] = {
    val instr = IOUtils.toString(in, defaultEncoding)
    val entData = EntityData.Ser.p(instr)
    entData.entity_details.entMap
  }

  def extract(path:String):Map[String, EntDetail] = processAllFiles(path, extractOne).reduce(_ ++ _)

  val entMap = extract("E:\\VMShare\\malware-161126-12.tgz")

  val orderedByDate = entMap.toIndexedSeq.filter(_._2.curated == 1).sortBy(_._2.created)(Ordering[Option[String]].reverse).take(200)
  println(orderedByDate.map(_._2).mkString("\n"))
}
