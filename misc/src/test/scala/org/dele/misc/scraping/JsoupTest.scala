package org.dele.misc.scraping

/**
  * Created by dele on 2016-12-09.
  */
import org.jsoup._

object JsoupTest extends App {

  val ua = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.99 Safari/537.36"

  val doc = Jsoup.connect("google.com")

  println("shit")
}
