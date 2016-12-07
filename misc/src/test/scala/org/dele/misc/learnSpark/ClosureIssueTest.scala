package org.dele.misc.learnSpark

/**
  * Created by jiaji on 12/3/2016.
  */
object ClosureIssueTest extends App {
  import SparkTestUtil._
  def calcCount(d:Array[Int]):Int = {
    var count = 0
    val rdd = context.parallelize(d)

    rdd.foreach{ x =>
      count += x
    }

    count // 0
  }

  def calcCount1(d:Array[Int]):Long = {
    val rdd = context.parallelize(d)
    val acc = context.longAccumulator("my acc")

    rdd.foreach{ x =>
      acc.add(x)
      println("------------------- Current Accumulator value (undefine behavior!) -------------------------")
      println(acc.value) // undefined results!!
    }

    acc.value // 0
  }

  val in = Array(2, 3, 4, 5, 6, 7)
  println(calcCount(in))
  println("using accumulator: ")
  println(calcCount1(in))
}
