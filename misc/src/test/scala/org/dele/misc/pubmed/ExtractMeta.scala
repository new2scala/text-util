package org.dele.misc.pubmed

import java.io.{FileOutputStream, InputStream}
import java.nio.charset.StandardCharsets

import org.apache.commons.io.IOUtils
import org.dele.misc.tgz.TgzUtil

import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag
import scala.xml.NodeSeq

/**
  * Created by dele on 2017-02-22.
  */
object ExtractMeta extends App {
  import scala.xml

  import PubmedXmlHelpers._


  def readOne(xmlStr:String):String = {
    import org.json4s.Xml.toJson
    import org.json4s.jackson.JsonMethods._

    extractFromXml(xmlStr)

    /*
    val x = xml.XML.loadString(xmlStr)
    val authors = x \ "MedlineCitation" \ "Article" \ "AuthorList" \ "Author"
    authors.foreach{ author =>
      val n = author \ "LastName"
      println(n.text)
    }
    val j = toJson(x)
    pretty(j)
    */
    ""
  }

  type PubmedArticleXmlProcessor[T] = (String => T)


  def processAll[T:ClassTag](s:InputStream, articleProcessor: PubmedArticleXmlProcessor[T]):Array[T] = {
    /*
    val buf = new Array[Byte](4096)
    s.read(buf)
    */
    val str = IOUtils.toString(s, StandardCharsets.UTF_8)
    var results = ListBuffer[T]()
    var buf = ListBuffer[String]()
    str.lines.foreach { line =>
      val trim = line.trim
      if (trim == "<PubmedArticle>") {
        buf = ListBuffer[String]()
      }

      buf += trim

      if (trim == "</PubmedArticle>") {
        results += articleProcessor(buf.mkString("\n"))
      }
    }

    results.toArray

    //writeInBatch(resultJsons.toArray, 3000, "/home/dele/tmp/medline17n0885")
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

  def findFirst(is:InputStream):Unit = {

  }

  //TgzUtil.processGzFile("/home/dele/tmp/medline17n0001.xml.gz", showText)

  def extractFromXml(xmlStr:String):_AuthorList = {
    val x:NodeSeq = xml.XML.loadString(xmlStr)
    val authorListNode = x \ "MedlineCitation" \ "Article" \ "AuthorList"

    xml2AuthorList(authorListNode)
    //_PubmedArticle(null, null)
  }

  def readAllAuthors(is:InputStream) = {
    val authorLists = processAll(is, extractFromXml)
    val allAuthors = authorLists.flatMap(_.Authors)
    allAuthors
  }

  def dumpAuthors(outputFile:String) = {
    val str = allAuthors.flatMap(_.name2String).distinct.sorted.mkString("\n")
    val ofs = new FileOutputStream(outputFile)
    IOUtils.write(str, ofs, StandardCharsets.UTF_8)
  }

  val allAuthors = TgzUtil.processGzFile("/home/dele/tmp/medline17n0885.xml.gz", readAllAuthors)
  dumpAuthors("/home/dele/tmp/medline17n0885.authors.distinct.txt")
}
