package org.dele.text.maen

import org.dele.text.maen.test.TestAtom.Atom
import org.scalatest.ShouldMatchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test

/**
  * Created by jiaji on 2016-02-09.
  */
class PropMatcherLibTest extends TestNGSuite with ShouldMatchers with TableDrivenPropertyChecks {
  import AtomPropMatcherLib._
  import TestHelper._

  val paramData = Array(Array("data"))

  val compParamData = Array(Array("F", "PoS"), Array("word1", "word2"), Array("pos1", "pos2"))

  val testData = Table(
    ("matcherTmplId", "spawnParams", "atomToTest", "matchResult"),
    ("Comp", compParamData, Atom("word2", Map("PoS-tag" -> Set("pos2"))), true),
    ("Comp", compParamData, Atom("word1", Map("PoS-tag" -> Set("pos2"))), true),
    ("Comp", compParamData, Atom("word2", Map("PoS-tag" -> Set("pos1"))), true),
    ("Comp", compParamData, Atom("word2", Map("PoS-tag" -> Set("pos3"))), false),
    ("Comp", compParamData, Atom("word3", Map("PoS-tag" -> Set("pos1"))), false),
    ("F", paramData, Atom("data", Map()), true),
    ("E", Array(Array("Company")), Atom("Microsoft", Map("entityType" -> Set("Entity", "OrgEntity", "Company"))), true),
    ("E", Array(Array("Company", "Organization")), Atom("FBI", Map("entityType" -> Set("Entity", "OrgEntity", "Organization"))), true),
    ("C", paramData, Atom("Data", Map()), false),
    ("C", Array(Array("Data")), Atom("Data", Map()), true),
    ("L", paramData, Atom("txt", Map("lemma" -> Set("data"))), true),
    ("L", paramData, Atom("txt", Map("lemma" -> Set("data", "date"))), true),
    ("EA", Array(Array("Organization"), Array("category"), Array("ThreatActor")), Atom("txt", Map("entityType" -> Set("Entity", "OrgEntity", "Organization"), "category" -> Set("ThreatActor"))), true),
    ("EAx", Array(Array("Organization"), Array("category"), Array("ThreatActor")), Atom("txt", Map("entityType" -> Set("Entity", "OrgEntity", "Organization"), "category" -> Set("ThreatActor"))), false)
  )

  @Test
  def testSimple = {

    forAll(testData) {
      (matcherTmplId, spawnParams, atomToTest, matchResult) => {
        val pm = PropMatcherTmplMap(matcherTmplId).spawn(spawnParams, EmptyRegexDict)
        pm.get.check(atomToTest) shouldBe(matchResult)

      }
    }

  }

  val compositeTestData = Table(
    ("pmList", "atom2Test", "matchResult"),
    (
      List(("E", Array(Array("Organization"))), ("EA", Array(Array("Organization"), Array("category"), Array("ThreatActor")))),
      Atom("Lizard Squad", Map("entityType" -> Set("Entity", "OrgEntity", "Organization"), "category" -> Set("ThreatActor"))),
      true
      )
  )

  import TAtomMatcher._
  @Test
  def testComposite = {
    forAll(compositeTestData) {
      (pmList, atom2Test, matchResult) => {
        val l = pmList.map(p => PropMatcherTmplMap.get(p._1).get.spawn(p._2, EmptyRegexDict))
        l.exists(_.isFailure) shouldBe(false)
        val m = composite(l.map(_.get))
        m.check(atom2Test) shouldBe(matchResult)
      }
    }
  }

}
