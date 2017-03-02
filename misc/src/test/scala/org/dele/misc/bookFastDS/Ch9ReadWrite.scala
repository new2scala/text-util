package org.dele.misc.bookFastDS

import org.apache.spark.sql.SparkSession

/**
  * Created by dele on 2017-03-02.
  */
object Ch9ReadWrite extends App {

  val spark = SparkSession.builder()
    .master("local[*]")
    .appName("Read Write tests")
    .config("spark.logConf", "true")
    .config("spark.logLevel", "ERROR")
    .getOrCreate()


  val cars = spark.read.option("header", "true").csv("res/data/cars.csv")

  println(cars.count())
  cars.printSchema()
  cars.show(5)

  cars.write.mode("overwrite").option("header", "true").csv("res/data/cars-out.csv")
  cars.write.mode("overwrite").partitionBy("year").parquet("res/data/cars-out-parquet")

  spark.close()

}
