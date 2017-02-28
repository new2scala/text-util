package org.dele.misc.bookFastDS

import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.types.StructType

/**
  * Created by dele on 2017-02-28.
  */
object EmployeeTests extends App {
  val spark = SparkSession.builder()
    .appName("Employees test")
    .master("local[*]")
    .getOrCreate()
  import spark.implicits._

  case class Employee(EmployeeID : String,
                      LastName : String, FirstName : String, Title : String,
                      BirthDate : String, HireDate : String,
                      City : String, State : String, Zip : String,  Country : String,
                      ReportsTo : String)

  case class Order(OrderID : String, CustomerID : String, EmployeeID : String,
                   OrderDate : String, ShipCountry : String)

  case class OrderDetails(OrderID : String, ProductID : String, UnitPrice : Double,
                          Qty : Int, Discount : Double)

  val employees = spark.read.option("header", "true").csv("res/data/employee.csv").as[Employee]

  println(employees.count())

  employees.show(5)
  employees.head()

  employees.createOrReplaceTempView("EmployView")
  val emp1 = spark.sql("SELECT * FROM EmployView WHERE state='WA'")

  emp1.show(4)
  emp1.head(3)
  emp1.explain(true)

  val orders = spark.read.option("header", "true").csv("res/data/NW-Orders.csv").as[Order]
  orders.show(5)

  //import org.apache.spark.sql.catalyst.ScalaReflection
  //val schema = ScalaReflection.schemaFor[OrderDetails].dataType.asInstanceOf[StructType]
  val orderDetails = spark.read
    //.schema(schema)
    .option("inferSchema", "true")
    .option("header", "true")
    .csv("res/data/NW-Order-Details.csv").as[OrderDetails]
  //orderDetails.printSchema()
  orderDetails.show(5)

  spark.close()
}
