package org.dele.misc.learnSpark

import org.apache.spark.{SparkConf, SparkContext}

/**
  * Created by jiaji on 12/3/2016.
  */
object BasicSetupTest extends App {

  val spaceSplitter = "\\s+"
  def strFunc1(line:String):String = {
    val words = line.trim.split(spaceSplitter).filter(_.nonEmpty)
    if (words.length > 0) s"Line (${line.length}): ${words(0)} ..."
    else "[Empty line!]"
  }

  import SparkTestUtil._
  def lineTest1 = {
    val rdd = context.textFile("LICENSE")
    val lineTransform1 = rdd.map(strFunc1).collect()
    println(lineTransform1)
  }

  def fileTest1 = {
    val rdd = context.textFile("LICENSE")
    val _words = rdd.flatMap(_.split(spaceSplitter))
    val words = _words.filter(_.nonEmpty)
    val pairWithCount = words.map((_, 1))
    val wordCount = pairWithCount.reduceByKey(_ + _)
    println(wordCount.toDebugString)
    val top15 = wordCount.takeOrdered(15)(Ordering[Int].reverse.on(_._2))
    println(top15)
  }

  fileTest1
}
