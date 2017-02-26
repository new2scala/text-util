package org.dele.misc.bookMasterSpark2.scalaExamples

import org.apache.spark.sql.SparkSession

import scala.util.Random

/**
  * Created by dele on 2017-02-26.
  */
object GroupByTest {
  def main(args:Array[String]): Unit = {
    val spark = SparkSession.builder()
      .appName("GroupBy test")
      .master("local[*]")
      .getOrCreate()

    val numMappers = 2
    val numKVPairs = 1000
    val valSize = 1000
    val numReducers = numMappers

    val pairs1 = spark.sparkContext.parallelize(
      0 until numMappers,
      numMappers
    ).flatMap{ p =>
      val ranGen = new Random()
      val arr = new Array[(Int, Array[Byte])](numKVPairs)
      (0 until numKVPairs).foreach{ idx =>
        val byteArr = new Array[Byte](valSize)
        ranGen.nextBytes(byteArr)
        arr(idx) = (math.abs(ranGen.nextInt()) % 500, byteArr)
      }
      arr
    }.cache()

    pairs1.count()

    val groups = pairs1.groupByKey(numReducers)
    val groupCount = groups.count()
    println(groupCount)

    val keyedGroups = groups.take(groupCount.toInt)

    println(keyedGroups.length)

    spark.stop()

  }
}
