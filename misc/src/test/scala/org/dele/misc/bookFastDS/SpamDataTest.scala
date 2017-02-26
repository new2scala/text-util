package org.dele.misc.bookFastDS

import org.apache.spark.sql.SparkSession

/**
  * Created by dele on 2017-02-26.
  */
object SpamDataTest extends App {

  val spark = SparkSession.builder()
    .appName("GroupBy test")
    .master("local[*]")
    .getOrCreate()

  val inFile = spark.sparkContext.textFile("res/data/spam.data")

  println(inFile.count())

  spark.stop()

}
