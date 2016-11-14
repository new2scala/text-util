package org.dele.text.maen.matchers

import org.scalatest.ShouldMatchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test

/**
  * Created by jiaji on 2016-04-25.
  */
class StoppedByMatcherManagerTest extends TestNGSuite with ShouldMatchers with TableDrivenPropertyChecks {
  import StoppedByMatcherManager._

  val testData = Table(
    ("stoppedByMap", "reverseMap"),

    (Iterable("l1" -> List(StoppedByConfig("sl1", true), StoppedByConfig("sl2", true))), Map("sl1" -> (true -> List("l1")), "sl2" -> (true -> List("l1")))),
    (
      Iterable("l1" -> List(StoppedByConfig("sl1", true), StoppedByConfig("sl2", true)), ("l1" -> List(StoppedByConfig("sl3", false)))),
      Map("sl1" -> (true -> List("l1")), "sl2" -> (true -> List("l1")), "sl3" -> (false -> List("l1")))
    )
  )

  @Test
  def t1 = {
    forAll(testData) {
      (stoppedByMap, reverseMap) => {
        val m = new StoppedByMatcherManager(stoppedByMap)
        reverseMap.foreach(
          p => {
            val k = p._1
            val expected = p._2
            m.getListsStopBy(k).get shouldBe expected
          }
        )
      }
    }
  }

}
