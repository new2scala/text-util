package org.dele.text.lapa.patterns

import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test

/**
  * Created by jiaji on 2016-08-19.
  */
class PtnNodeTest extends TestNGSuite with ShouldMatchers with TableDrivenPropertyChecks {
  import TLangPattern._

  val EmptyParamArray = Array[Array[String]]()

  def TwoDimArray(p1:String*):Array[Array[String]] = p1.map(Array(_)).toArray

  val parseTestData = Table(
    ("content", "entries", "params"),
    ("list1 list2", List("list1", "list2"), List(EmptyParamArray, EmptyParamArray)),
    ("A_B(l1|l2) A_of_B(m1|m2)", List("A_B", "A_of_B"), List(TwoDimArray("l1", "l2"), TwoDimArray("m1", "m2"))),
    ("list1 A_B(m1|m2)", List("list1", "A_B"), List(EmptyParamArray, TwoDimArray("m1", "m2")))
  )

  @Test
  def testParseContent = {
    forAll(parseTestData) { (content, entries, params) =>
      val r = parseNodeContent(content)
      r.map(_._1) shouldBe entries
      val actualParams = r.map(_._2)
      val params2 = params.map(_.map(_.toIndexedSeq).toIndexedSeq)
      val actualParam2 = actualParams.map(_.paras.map(_.toIndexedSeq).toIndexedSeq)
      actualParam2 shouldBe(params2)
    }
  }
}
