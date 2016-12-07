package org.dele.misc.learnSpark

/**
  * Created by jiaji on 12/4/2016.
  */
object BroadcastVarTest extends App {
  import SparkTestUtil._
  val brList = context.broadcast(List("a", "b", "d"))
  val rdd = context.parallelize(List("12", "23", "34", "45"))
  val rlists = rdd.map(brList.value ::: _ :: Nil).collect()

  println(rlists.mkString("\n"))

}
