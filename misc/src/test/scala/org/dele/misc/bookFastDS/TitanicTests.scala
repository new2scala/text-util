package org.dele.misc.bookFastDS

import org.apache.spark.sql.SparkSession

/**
  * Created by dele on 2017-03-03.
  */
object TitanicTests extends App {

  val spark = SparkSession.builder()
    .appName("titanic")
    .master("local[*]")
    .getOrCreate()

  val passenges = spark.read.option("header", "true").csv("res/data/titanic3_02.csv")

  passenges.show(5)

  val pv = passenges.select("Pclass","Survived","Gender","Age","SibSp","Parch","Fare")

  pv.stat.crosstab("Survived", "Gender").show()

  pv.stat.crosstab("Survived", "Pclass").show()

  val agev = pv.select(pv("Survived"), (pv("Age")-pv("Age")%10).as("AgeBucket"))

  agev.show(3)
  
  agev.stat.crosstab("Survived", "AgeBucket").show()

  spark.close()
}
