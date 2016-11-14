package org.dele.text.lapa.patterns

import DomainStructure._
import org.scalatest.ShouldMatchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test

/**
  * Created by jiaji on 2016-02-19.
  */
class DomainStructureTest extends TestNGSuite with ShouldMatchers with TableDrivenPropertyChecks {

  import org.dele.text.lapa.TestHelper._
  @Test
  def t1 = {
    val ds = domainStructure.children
    ds.size shouldBe(3)
  }

  @Test
  def t2 = {
    val d = engDomainMgr.queryDomainId("cyber-attack", "entity-list-connector-words")
    d shouldBe(Option("_root_"))
  }

  @Test
  def t3 = {
    val ds = load(domainTree, List("cyber-attack"))
    ds.children.size shouldBe 2
    ds.children.exists(_.id == "cyber-attack") shouldBe false

    val ds2 = load(domainTree, List("military-maneuver", "-online-attack"))
    ds2.children.size shouldBe 2
    ds2.children.exists(_.id == "-online-attack") shouldBe false
  }

}
