package org.dele.misc.bookFastDS

import org.apache.spark.SparkFiles
import org.apache.spark.sql.SparkSession

/**
  * Created by dele on 2017-02-26.
  */
object SpamDataTest extends App {

  val spark = SparkSession.builder()
    .appName("GroupBy test")
    .master("local[*]")
    .getOrCreate()

  //val inFile = spark.sparkContext.textFile("res/data/spam.data")
  val f = spark.sparkContext.addFile("res/data/spam.data")
  val inFile = spark.sparkContext.textFile(SparkFiles.get("spam.data"))

  println(inFile.count())

  val nums = inFile.map { line =>
    line.split("\\s+").map(_.toDouble)
  }
  println(inFile.first())
  println(nums.first().mkString(", "))

  println(spark.sparkContext.getConf.toDebugString)

  println(spark.sparkContext.listFiles())

  spark.stop()

}
