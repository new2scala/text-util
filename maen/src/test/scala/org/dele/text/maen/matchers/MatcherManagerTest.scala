package org.dele.text.maen.matchers

import org.dele.text.maen.AtomPropMatcherLib._
import org.dele.text.maen.TInput
import org.dele.text.maen.TestHelper._
import org.dele.text.maen.matchers.MatcherManager.ContextChecker
import org.dele.text.maen.matchers.TMatcher._
import org.dele.text.maen.TInput
import org.scalatest.ShouldMatchers
import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test

/**
  * Created by jiaji on 2016-02-14.
  */
class MatcherManagerTest extends TestNGSuite with ShouldMatchers {
  import org.dele.text.maen.test.TestAtom._
  import org.dele.text.maen.test.TestInput._
  val input:TInput = fromAtomArrayEng(IndexedSeq(
    textAtom("Now"),
    FBI,
    textAtom("and"),
    Microsoft
  ))


  implicit val smclib = EmptySubMatchCheckerLib

  import SubMatchCheckerLib._
  val orgCompanyMatcherId = "org_company"
  val entityMatcher = fromAtomMatcher(E(EmptyRegexDict, Array("Company", "Organization")), EmptyCheckerIds, Option(orgCompanyMatcherId))
  val queryOrgCmpMatcher = queryPoolMatcher(orgCompanyMatcherId)
  val andMatcher = fromAtomMatcher(FExact("and"))
  val announceMatcher = fromAtomMatcher(FExact("announce"))
  val matcherId = Option("org_company_list")
  val matchers = matchersOrderedAllPositive(Seq(queryOrgCmpMatcher, andMatcher, queryOrgCmpMatcher), EmptyCheckerIds, matcherId)
  val notOrgMatcherId = Option("not_org")
  val notOrgMatcher = matchersOrderedAllPositive(Seq(entityMatcher, announceMatcher), List("Lng"), notOrgMatcherId)(StaticSubMatchCheckerLib)


  @Test
  def t1 = {
    val mm = MatcherManager.create

    mm.add(entityMatcher)
    mm.add(matchers)

    val resultPool = mm.m(input, EmptySubMatchCheckerLib, MatcherManager.EmptyMIdFilters)
    val orgCompany = resultPool.query(orgCompanyMatcherId)
    orgCompany.size shouldBe(2)

    val orgCompanyList = resultPool.query(matcherId.get)
    orgCompanyList.size shouldBe(1)
  }

  @Test
  def testExcludeFilter = {
    val mm = MatcherManager.create

    mm.add(entityMatcher)
    mm.add(matchers)

    val midFilter = (mid:MId) => mid == orgCompanyMatcherId
    val r1 = mm.m(input, EmptySubMatchCheckerLib, Set(midFilter))
    val q1 = r1.query(orgCompanyMatcherId)
    q1.size shouldBe(0)

    val midFilter2 = (mid:MId) => mid == matcherId.get
    val r2 = mm.m(input, EmptySubMatchCheckerLib, Set(midFilter2))
    val q2 = r2.query(orgCompanyMatcherId)
    q2.size shouldBe(2)
    val q3 = r2.query(matcherId.get)
    q3.size shouldBe(0)


  }


  @Test
  def testQ2 = {
    val mm = MatcherManager.create

    val m1id = Option("m1id")
    val m2id = Option("m2id")
    val m1 = matchersOrderedAllPositive(Seq(queryOrgCmpMatcher, andMatcher, queryOrgCmpMatcher), EmptyCheckerIds, m1id)
    val m2 = fromAtomMatcher(FExact("and"), EmptyCheckerIds, m2id)
    val mq2id = Option("mq2id")
    val mq2 = queryAnd(Set(m1id.get), Set(m2id.get), EmptyCheckerIds, mq2id)

    mm.add(entityMatcher)
    mm.add(m1)
    mm.add(m2)
    mm.add(mq2)

    val resultPool = mm.m(input, EmptySubMatchCheckerLib, MatcherManager.EmptyMIdFilters)
    val q2m = resultPool.query(mq2id.get)

    q2m.size shouldBe(1)
  }


  @Test(enabled = false)
  def testRemoveAlmostDup = {
    val mm = MatcherManager.create

    val m1id = Option("m1id")
    val m11id = Option("m11id")
    val m12id = Option("m12id")
    val m2id = Option("m2id")
    val m11 = matchersOrderedAllPositive(Seq(queryOrgCmpMatcher, andMatcher, queryOrgCmpMatcher), EmptyCheckerIds, m11id)
    val m12 = matchersOrderedAllPositive(Seq(queryOrgCmpMatcher, matchersOrderedAllPositive(Seq(andMatcher, queryOrgCmpMatcher))), EmptyCheckerIds, m12id)
    val m1 = matchersOR(m1id, Seq(queryPoolMatcher(m11id.get), queryPoolMatcher(m12id.get)))
    val m2 = fromAtomMatcher(FExact("and"), EmptyCheckerIds, m2id)
    val mq2id = Option("mq2id")
    val mq2 = queryAnd(Set(m11id.get), Set(m2id.get), EmptyCheckerIds, mq2id)

    mm.add(entityMatcher)
    mm.add(m1)
    mm.add(m11)
    mm.add(m12)
    mm.add(m2)
    mm.add(mq2)

    val resultPool = mm.m(input, EmptySubMatchCheckerLib, MatcherManager.EmptyMIdFilters)
    val q2m = resultPool.query(m1id.get)

    q2m.size shouldBe(1)
  }


  import StoppedByMatcherManager._
  val stoppedByMgr = new StoppedByMatcherManager(Map(orgCompanyMatcherId -> List(StoppedByConfig(notOrgMatcherId.get, true))))

  val input1:TInput = fromAtomArrayEng(IndexedSeq(
    textAtom("Now"),
    FBI,
    textAtom("and"),
    Microsoft,
    textAtom("announce")
  ))

  @Test
  def testWithStoppedByMatchers = {
    implicit val sbmm = stoppedByMgr
    val mm = MatcherManager.create

    mm.add(entityMatcher)
    mm.add(matchers)
    mm.add(notOrgMatcher)
    mm.setStoppedByManager(stoppedByMgr)

    val resultPool = mm.m(input1, EmptySubMatchCheckerLib, MatcherManager.EmptyMIdFilters)
    val orgCompany = resultPool.query(orgCompanyMatcherId)
    orgCompany.size shouldBe(1)
    val notOrgCompany = resultPool.query(notOrgMatcherId.get)
    notOrgCompany.size shouldBe(1)

  }

  val contextChecker:ContextChecker = (ctxt, mid) => {
    mid != notOrgMatcherId.get
  }
  val stoppedByMgr1 = new StoppedByMatcherManager(Map(orgCompanyMatcherId -> List(StoppedByConfig(notOrgMatcherId.get, true))), contextChecker)
  val input2:TInput = fromAtomArrayEng(IndexedSeq(
    textAtom("Now"),
    FBI,
    textAtom("and"),
    Microsoft,
    textAtom("announce")
  ))


  @Test
  def testWithContextCheck = {
    implicit val sbmm = stoppedByMgr1
    val mm = MatcherManager.create

    mm.add(entityMatcher)
    mm.add(matchers)
    mm.add(notOrgMatcher)
    mm.setStoppedByManager(stoppedByMgr1)

    val resultPool = mm.m(input2, EmptySubMatchCheckerLib, MatcherManager.EmptyMIdFilters)
    val orgCompany = resultPool.query(orgCompanyMatcherId)
    orgCompany.size shouldBe(2)
    val notOrgCompany = resultPool.query(notOrgMatcherId.get)
    notOrgCompany.size shouldBe(0)

  }
}
