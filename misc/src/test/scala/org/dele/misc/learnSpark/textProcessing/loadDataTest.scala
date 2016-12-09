package org.dele.misc.learnSpark.textProcessing

/**
  * Created by dele on 2016-12-07.
  */
object loadDataTest extends App {

  import org.dele.misc.learnSpark.SparkTestUtil._

  val path = "/home/dele/projs/text-util/data/20news-bydate-train/*"

  val rdd = context.wholeTextFiles(path)
  val text = rdd.map{ case (file, text) => text }
  println(text.count)
}
