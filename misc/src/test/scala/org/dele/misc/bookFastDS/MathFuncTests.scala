package org.dele.misc.bookFastDS

import org.apache.spark.sql.{Dataset, SparkSession}

/**
  * Created by dele on 2017-03-03.
  */
object MathFuncTests extends App {
  val spark = SparkSession.builder()
    .appName("math functions")
    .master("local[*]")
    .getOrCreate()

  import spark.implicits._
  val ds = spark.createDataset(List(0, 10, 100, 1000))

  import org.apache.spark.sql.functions._

  val col = ds("value")
  val ds1 = ds.select(col, log(col), log1p(col), log10(col), sqrt(col))

  ds1.show()

  val hypotdata = spark.read.option("inferSchema", "true").option("header", "true").csv("res/data/hypot.csv")

  val (colx, coly) = (hypotdata("X"), hypotdata("Y"))
  val hd1 = hypotdata.select(colx, coly, hypot(colx, coly))

  hd1.show(5)

  spark.close()
}
