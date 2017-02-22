package org.dele.misc.pubmed

import java.io.{FileOutputStream, InputStream}
import java.nio.charset.StandardCharsets

import org.apache.commons.io.IOUtils
import org.dele.misc.tgz.TgzUtil

import scala.collection.mutable.ListBuffer

/**
  * Created by dele on 2017-02-22.
  */
object ExtractMeta extends App {

  def showText(s:InputStream):Unit = {
    /*
    val buf = new Array[Byte](4096)
    s.read(buf)
    */
    def readOne(xmlStr:String):String = {
      import org.json4s.Xml.toJson
      import org.json4s.jackson.JsonMethods._
      import scala.xml
      val x = xml.XML.loadString(xmlStr)
      val j = toJson(x)
      return pretty(j)
    }

    val str = IOUtils.toString(s, StandardCharsets.UTF_8)
    var resultJsons = ListBuffer[String]()
    var buf = ListBuffer[String]()
    str.lines.foreach { line =>
      val trim = line.trim
      if (trim == "<PubmedArticle>") {
        buf = ListBuffer[String]()
      }

      buf += trim

      if (trim == "</PubmedArticle>") {
        resultJsons += readOne(buf.mkString("\n"))
      }
    }
    val os = new FileOutputStream("/home/dele/tmp/med.json")
    IOUtils.write(resultJsons.mkString("\n"), os, StandardCharsets.UTF_8)
  }

  TgzUtil.processGzFile("/home/dele/tmp/med.gz", showText)
}
