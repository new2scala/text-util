package org.dele.misc.bookFastDS

import org.apache.spark.sql.types._
import org.apache.spark.sql.{DataFrame, Dataset, SparkSession}

/**
  * Created by dele on 2017-02-27.
  */
object GeoCityDataTest extends App {

  case class GeoLocEntry(
                     locId:Int,
                     country:String,
                     region:String,
                     city:String,
                     postalCode:String,
                     latitude:Double,
                     longitude:Double,
                     metroCode:String,
                     areaCode:String
                   )

  val spark = SparkSession.builder()
    .appName("GeoCity")
    .master("local[*]")
    .getOrCreate()

  //val inFile = spark.sparkContext.textFile(
  import spark.implicits._
/*
  val customSchema = StructType(Array(
    StructField("locId", IntegerType),
    StructField("country", StringType),
    StructField("region", StringType),
    StructField("city", StringType),
    StructField("postalCode", StringType),
    StructField("latitude", DoubleType),
    StructField("longitude", DoubleType),
    StructField("metroCode", StringType),
    StructField("areaCode", StringType)
  ))
*/
  import org.apache.spark.sql.catalyst.ScalaReflection
  val customSchema = ScalaReflection.schemaFor[GeoLocEntry].dataType.asInstanceOf[StructType]
  val df:DataFrame = spark.read
    .option("header", "true")
    .schema(customSchema)
    .csv("/home/dele/tmp/GeoLiteCity_20170207/GeoLiteCity-Location.csv")
    //.csv("res/data/g1.csv")


  val ds = df.as[GeoLocEntry]
  println(ds.count())

  val countries = ds.map(_.country).distinct().collect()
  val countryMap = countries.indices.map(idx => countries(idx) -> idx).toMap
  spark.sparkContext.broadcast(countryMap)

  val mapped = ds.map{ geo =>
    val idx = countryMap(geo.country)
    idx -> geo
  }.collect()

  println(mapped)

  spark.close()
}
