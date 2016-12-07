package org.dele.misc.learnSpark

import org.apache.spark.{SparkConf, SparkContext}

/**
  * Created by jiaji on 12/3/2016.
  */
object SparkTestUtil {
  val conf = new SparkConf()
    .setAppName("basic setup app")
    .setMaster("local[4]")

  lazy val context = new SparkContext(conf)

}
