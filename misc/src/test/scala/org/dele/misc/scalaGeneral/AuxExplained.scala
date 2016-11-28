package org.dele.misc.scalaGeneral

/**
  * Created by jiaji on 11/28/2016.
  */
object AuxExplained extends App{

  trait TraitWithIssue[TIn] {
    type TResult
    def result: TResult
  }

  class C1(x:String) extends TraitWithIssue[Boolean] {
    type TResult = String
    def result = x
  }

  class C2(x:Int) extends TraitWithIssue[Int] {
    type TResult = Int
    def result = x
  }

  def test1:Unit = {
    val c1 = new C1("c1")
    println(c1.result)
    val c2 = new C2(12)
    println(c2.result)
  }

  test1

  def test2[T](t:TraitWithIssue[T]):List[t.TResult] = t.result::List(t.result)

  println(test2(new C2(15)))

  def test3[T](t:TraitWithIssue[T])(implicit tr:t.TResult):List[t.TResult] = tr::List(t.result)

  def runTest3:Unit = {
    implicit val x = "xx1"
    println(test3(new C1("cc1")))
  }

  runTest3

  // doesn't compile:
  //def test4[T](t:TraitWithIssue[T], tr:t.TResult):List[t.TResult] = tr::List(t.result)

  type Aux[TIn,TRes] = TraitWithIssue[TIn] {
    type TResult = TRes
  }
  def test4[TIn, TRes](t:Aux[TIn,TRes], tr:TRes):List[TRes] = tr::List(t.result)

  println(test4(new C1("c1"), "x1"))
  // doesn't compile
  //println(test4(new C2(15), "x2"))
  println(test4(new C2(15), 2))
}
