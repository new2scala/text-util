package org.dele.misc.bookFastDS

import org.apache.spark.sql.SparkSession

/**
  * Created by dele on 2017-03-02.
  */
object CarMileageTest extends App {

  val spark = SparkSession.builder()
    .appName("car mileage")
    .master("local[*]")
    .config("logConf", "true")
    .getOrCreate()

  val milData = spark.read.option("header", "true").option("inferSchema", "true").csv("res/data/car-milage.csv")

  println(milData.count())
  milData.show(5)
  milData.printSchema()
  milData.describe("mpg", "hp", "torque", "automatic").show()

  milData.groupBy("automatic").avg("mpg", "hp", "torque").show()
  milData.groupBy().avg("mpg", "hp", "torque").show()

  import org.apache.spark.sql.functions._
  milData.agg(stddev(milData("mpg")), avg(milData("torque"))).show()

  spark.close()
}
