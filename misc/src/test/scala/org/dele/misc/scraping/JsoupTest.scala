package org.dele.misc.scraping

/**
  * Created by dele on 2016-12-09.
  */

import jdk.nashorn.internal.parser.JSONParser
import org.json4s.DefaultFormats
import org.jsoup._

import collection.JavaConverters._
import org.json4s.jackson.JsonMethods._
object JsoupTest extends App {

  val ua = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.99 Safari/537.36"
  val searchTerm = "whitebacked planthopper"
  val searchUrl = "https://www.google.com/search?site=imghp&tbm=isch&source=hp&biw=1920&bih=955&q=" + searchTerm.replace(" ", "+") + "&gws_rd=cr";

  val doc = Jsoup.connect(searchUrl)
    .userAgent(ua)
    .referrer("https://www.google.com/").get()

  implicit val fmt = DefaultFormats
  doc.select("div.rg_meta").asScala.foreach{ elem =>
    val src = elem.text()
    val json = parse(src)
    val url = (json \ "ou")
    if (url != null) {
      val out = url.extract[String]
      println(out)
    }
  }

  println("end")
}
