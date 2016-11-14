package org.dele.text.maen.matchers

import org.dele.text.maen.AtomPropMatcherLib._
import org.dele.text.maen.TestHelper._
import SubMatchCheckerLib._
import TMatcher._
import org.dele.text.maen.TInput
import org.dele.text.maen.test.TestAtom._
import org.dele.text.maen.test.TestInput._
import org.scalatest._
import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test

/**
  * Created by jiaji on 2016-10-04.
  */
class ANOBMatcherTest extends TestNGSuite with ShouldMatchers {
  val input:TInput = fromAtomArrayEng(IndexedSeq(
    textAtom("Now"),
    FBI,
    textAtom("and"),
    Microsoft,
    textAtom("announce")
  ))

  implicit val checkerLib = StaticSubMatchCheckerLib
  val orgCompanyMatcherId = "OrgCompany"
  val entityMatcher = fromAtomMatcher(E(EmptyRegexDict, Array("Company", "Organization")), EmptyCheckerIds, Option(orgCompanyMatcherId))
  val andMatcher = fromAtomMatcher(FExact("and"))
  val announceMatcher = fromAtomMatcher(FExact("announce"))

  @Test
  def t1 = {
    val mm = MatcherManager.create

    val anobMatcherId = "anob"
    val anobMatcherId2 = "anob2"
    val andentMatcherId = "andEnt"
    val entAnnounceMatcherId = "entAnnounce"
    val andEntMatcher = matchersOrderedAllPositive(Seq(andMatcher, entityMatcher), List(ListNGramId), Option(andentMatcherId))
    val entAnnounceMatcher = matchersOrderedAllPositive(Seq(entityMatcher, announceMatcher), List(ListNGramId), Option(entAnnounceMatcherId))
    val anobMatcher = matchersNonOverlap(andEntMatcher, andMatcher, Option(anobMatcherId))
    val anobMatcher2 = matchersNonOverlap(andEntMatcher, announceMatcher, Option(anobMatcherId2))
    mm.add(entityMatcher)
    mm.add(andMatcher)
    mm.add(andEntMatcher)
    mm.add(entAnnounceMatcher)
    mm.add(anobMatcher)
    mm.add(anobMatcher2)

    val resultPool = mm.m(input, StaticSubMatchCheckerLib, MatcherManager.EmptyMIdFilters)
    val orgCompanies = resultPool.query(orgCompanyMatcherId)
    orgCompanies.size shouldBe(2)

    val andentMatches = resultPool.query(andentMatcherId)
    andentMatches.size shouldBe(1)
    val anobMatches = resultPool.query(anobMatcherId)
    anobMatches.size shouldBe(0)
    val anobMatches2 = resultPool.query(anobMatcherId2)
    anobMatches2.size shouldBe(1)


  }

}
