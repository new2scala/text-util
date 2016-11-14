package org.dele.text.lapa.patterns

import TLangPattern.LangPatternGroupFromPatterns
import org.dele.text.maen.AtomPropMatcherLib._
import org.dele.text.maen.TInput
import org.dele.text.maen.extracts.Extract
import org.dele.text.maen.matchers.MatcherManager
import org.dele.text.maen.matchers.TMatcher._
import org.dele.text.maen.test.TestAtom._
import org.dele.text.lapa.TestHelper
import org.scalatest.ShouldMatchers
import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test

/**
  * Created by jiaji on 2016-02-15.
  */
class AppliedPatternsTest extends TestNGSuite with ShouldMatchers {
  import org.dele.text.lapa.TestHelper._
  import org.dele.text.maen.test.TestInput._

  val _extractDef =
    """
      |{
      | "extractDefSets": [
      |  {
      |   "domain": "-non-intrusive-online-attack",
      |   "extractDefs": [
      |    {
      |     "extractName": "_attacker",
      |     "matcherId": "ALL-ATTACKERS.OrgAttacker",
      |     "atomMatcherDef": "E(Organization)"
      |    },
      |    {
      |     "extractName": "_target",
      |     "matcherId": "ALL-TARGETS.OrgCmpTarget",
      |     "atomMatcherDef": "E(Company | Organization)"
      |    },
      |    {
      |     "extractName": "_indicator",
      |     "matcherId": "pattern1"
      |    }
      |   ]
      |  }
      | ]
      |}
    """.stripMargin
  val extractDefs = Extract.fromJson(_extractDef)
  val cyberEventExtractDefSet = extractDefs.getExtractDefSet("-non-intrusive-online-attack")

  implicit val smcLib = EmptySubMatchCheckerLib

  import DomainStructure._

  val LangEngZhs = Set("eng", "zhs")
  val LangZhs = Set("zhs")
  @Test
  def testPatternSupportedLang = {
    //val ptnId1 = engDomainMgr.getFullId(testPatternDomainId, testPatternId)
    val ptnGrp = patternDomain.patternGroupJsonById(testPatternId).get
    ptnGrp.languages shouldBe LangEngZhs
    val patternsGroup = ptnGrp.asInstanceOf[LangPatternGroupFromPatterns].toPatternGroup
    patternsGroup.patterns.forall(_.supportedLanguages == LangEngZhs) shouldBe true
    val ptnGrp2 = patternDomain.patternGroupJsonById("[METHOD]-attack").get
    val patternsGroup2 = ptnGrp2.asInstanceOf[LangPatternGroupFromPatterns].toPatternGroup
    patternsGroup2.languages shouldBe LangEngZhs
    patternsGroup2.patterns.forall(_.supportedLanguages == LangEngZhs) shouldBe true
    val ptnGrp3 = patternDomain.patternGroupJsonById("ALL-ATTACKERS").get
    val patternsGroup3 = ptnGrp3.asInstanceOf[LangPatternGroupFromPatterns].toPatternGroup
    patternsGroup3.languages shouldBe LangEngZhs
    patternsGroup3.patterns(0).supportedLanguages shouldBe LangEngZhs
    patternsGroup3.patterns(1).supportedLanguages shouldBe LangZhs
  }

  val LineTypeLNGAndClause1 = List("lngChecker", "clause1")
  val LineTypeLNG = List("lngChecker")
  @Test
  def testDefaultLineTypes = {
    //val ptnId1 = engDomainMgr.getFullId(testPatternDomainId, testPatternId)

    val ptnGrp2 = patternDomain.patternGroupJsonById("[METHOD]-attack").get
    val patternsGroup2 = ptnGrp2.asInstanceOf[LangPatternGroupFromPatterns].toPatternGroup
    patternsGroup2.defLineTypes shouldBe LineTypeLNGAndClause1
    patternsGroup2.patterns.forall(_.defaultLineTypes == LineTypeLNGAndClause1) shouldBe true
    val ptnGrp3 = patternDomain.patternGroupJsonById("ALL-ATTACKERS").get
    val patternsGroup3 = ptnGrp3.asInstanceOf[LangPatternGroupFromPatterns].toPatternGroup
    patternsGroup3.defLineTypes shouldBe LineTypeLNGAndClause1
    patternsGroup3.patterns(0).defaultLineTypes shouldBe LineTypeLNG
    patternsGroup3.patterns(1).defaultLineTypes shouldBe LineTypeLNGAndClause1

  }

  import LangPatternGroupTemplate._
  import Extract._
  @Test
  def t1 = {
    engAppliedPatterns.lang shouldBe("eng")

    val domainPatternId = engDomainMgr.getFullId(testPatternDomainId, testPatternId)
    val mm = MatcherManager.create
    engAppliedPatterns.applyDomain(engDomainMgr, TMatcherGen.NoMatcherTemplateLibGen, patternDomain, EmptyPatternTemplateLib, EmptyRegexDict).foreach(mm.add)
    //val domain = patternDomain.id
    //val orgList = d(domain, "org-list")
    //val orgCmpList = d(domain, "org-cmp-list")
    //val countryList = d(domain, "country-list")
    //val attackWordList = d(domain, "attack-words")
    //val againstWordList = d(domain, "against-words")
    //val launchWordList = d(domain, "launch-words")
    //engAppliedPatterns.applyPatternGroup(patternGroup2).foreach(mm.add)
    //engAppliedPatterns.applyPatternGroup(patternGroup3).foreach(mm.add)
    //engAppliedPatterns.applyPatternGroup(patternGroup4).foreach(mm.add)
    //mm.add(fromAtomMatcher(E(Array("Organization")), Option(orgList)))
    //mm.add(fromAtomMatcher(E(Array("Company", "Organization")), Option(orgCmpList)))
    //mm.add(fromAtomMatcher(E(Array("Country")), Option(countryList)))

    engDomainMgr.listMatchers(TMatcherGen.NoMatcherTemplateLibGen, EmptyRegexDict).foreach(mm.add)
    //mm.add(fromAtomMatcher(F("attack"), Option(attackWordList)))
    //mm.add(fromAtomMatcher(F("against"), Option(againstWordList)))
    //mm.add(fromAtomMatcher(F("launch"), Option(launchWordList)))


    val resultPool = mm.m(engInput1, EmptySubMatchCheckerLib, MatcherManager.EmptyMIdFilters)
    val events = resultPool.query(domainPatternId)
    events.foreach(
      m => {
        val ex = cyberEventExtractDefSet.run(m, EmptyRelatedEntityCheckerIds)
        println(ex.mkString(" "))
      }
    )


    val zhsmm = MatcherManager.create
    zhsAppliedPatterns.applyDomain(zhsDomainMgr, TMatcherGen.NoMatcherTemplateLibGen, patternDomain, EmptyPatternTemplateLib, EmptyRegexDict).foreach(zhsmm.add)
    //zhsAppliedPatterns.applyPatternGroup(patternGroup2).foreach(zhsmm.add)
    //zhsAppliedPatterns.applyPatternGroup(patternGroup3).foreach(zhsmm.add)
    //zhsAppliedPatterns.applyPatternGroup(patternGroup4).foreach(zhsmm.add)
    //zhsmm.add(fromAtomMatcher(E(Array("Organization")), Option(orgList)))
    //zhsmm.add(fromAtomMatcher(E(Array("Company", "Organization")), Option(orgCmpList)))
    //zhsmm.add(fromAtomMatcher(E(Array("Country")), Option(countryList)))
    //zhsmm.add(fromAtomMatcher(F("攻击"), Option(attackWordList)))
    //zhsmm.add(fromAtomMatcher(F("针对"), Option(againstWordList)))
    //zhsmm.add(fromAtomMatcher(F("发起"), Option(launchWordList)))
    zhsDomainMgr.listMatchers(TMatcherGen.NoMatcherTemplateLibGen, EmptyRegexDict).foreach(zhsmm.add)

    var zhsResultPool = zhsmm.m(zhsInput1, EmptySubMatchCheckerLib, MatcherManager.EmptyMIdFilters)
    var zhsEvents = zhsResultPool.query(domainPatternId)
    zhsEvents.foreach(
      m => {
        val ex = cyberEventExtractDefSet.run(m, EmptyRelatedEntityCheckerIds)
        println(ex.mkString(" "))
      }
    )
    zhsResultPool = zhsmm.m(zhsInput2, EmptySubMatchCheckerLib, MatcherManager.EmptyMIdFilters)
    zhsEvents = zhsResultPool.query(domainPatternId)
    zhsEvents.foreach(
      m => {
        val ex = cyberEventExtractDefSet.run(m, EmptyRelatedEntityCheckerIds)
        println(ex.mkString(" "))
      }
    )

  }
}
