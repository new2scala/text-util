package org.dele.misc.bookMasterSpark2

/**
  * Created by dele on 2017-02-26.
  */
object SparkSessionTests extends App {

  import org.apache.spark.sql.SparkSession

  val spark = SparkSession.builder()
    .appName("My first Spark App")
    .master("local[*]")
    //.enableHiveSupport()
  //    .config("spark.sql.warehouse.dir", "target/spark-warehouse")
    .getOrCreate()

  println(spark.version)

  import spark.implicits._

  val strs = spark.emptyDataset[String]
  strs.printSchema()

  val one = Seq(1, 2).toDS

  one.printSchema()

  spark.stop()

}
