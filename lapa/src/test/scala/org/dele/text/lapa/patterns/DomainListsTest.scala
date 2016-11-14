package org.dele.text.lapa.patterns

import DomainLists._
import org.scalatest.ShouldMatchers
import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test

/**
  * Created by jiaji on 2016-02-21.
  */
class DomainListsTest extends TestNGSuite with ShouldMatchers {

  import org.dele.text.lapa.TestHelper._
  @Test
  def t1 = {
    engDomainLists.domainLists.size shouldBe(3)
  }

}
