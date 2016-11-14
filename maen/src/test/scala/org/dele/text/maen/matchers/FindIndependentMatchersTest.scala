package org.dele.text.maen.matchers

import TMatcher.MId
import org.scalatest._
import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test

/**
  * Created by jiaji on 2016-09-18.
  */
class FindIndependentMatchersTest extends TestNGSuite with ShouldMatchers {

  @Test
  def t1:Unit = {
    val depMap = Map(
      "matcher1" -> Set("matcher11", "matcher12"),
      "matcher11" -> Set[MId](),
      "matcher12" -> Set("matcher121"),
      "matcher121" -> Set[MId](),
      "matcher2" -> Set("matcher21"),
      "matcher21" -> Set("matcher211", "matcher212"),
      "matcher211" -> Set("matcher2"),
      "matcher212" -> Set[MId]()
    )

    val (ind, rem) = MatcherManager.shuffleBasedOnDependency(depMap)
    rem shouldBe Set("matcher2", "matcher21", "matcher211")
  }
}
