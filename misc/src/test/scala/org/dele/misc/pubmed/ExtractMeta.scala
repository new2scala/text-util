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

  def readOne(xmlStr:String):String = {
    import org.json4s.Xml.toJson
    import org.json4s.jackson.JsonMethods._
    import scala.xml
    val x = xml.XML.loadString(xmlStr)
    val j = toJson(x)
    return pretty(j)
  }

  def showText(s:InputStream):Unit = {
    /*
    val buf = new Array[Byte](4096)
    s.read(buf)
    */
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

    writeInBatch(resultJsons.toArray, 3000, "/home/dele/tmp/medline17n0885")
  }

  private def writeInBatch(jsons:Array[String], batchSize:Int, baseName:String):Unit = {
    val batchCount = (jsons.length-1) / batchSize + 1
    var currData = jsons
    (0 until batchCount).foreach{ batchIdx =>
      val (currBatch, remaining) = if (currData.length > batchSize) currData.splitAt(batchSize) else (currData, Array[String]())

      val ofs = new FileOutputStream(f"$baseName$batchIdx%03d.json")
      IOUtils.write(currBatch.mkString("[", ",\n", "]"), ofs, StandardCharsets.UTF_8)
      ofs.close()

      currData = remaining
    }
  }

  TgzUtil.processGzFile("/home/dele/tmp/medline17n0885.xml.gz", showText)
}
