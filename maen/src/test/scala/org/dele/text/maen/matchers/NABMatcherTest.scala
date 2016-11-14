package org.dele.text.maen.matchers

import org.dele.text.maen.AtomPropMatcherLib._
import org.dele.text.maen.TInput
import org.dele.text.maen.TestHelper._
import org.dele.text.maen.matchers.SubMatchCheckerLib._
import org.dele.text.maen.matchers.TMatcher._
import org.dele.text.maen.test.TestAtom._
import org.dele.text.maen.test.TestInput._
import org.dele.text.maen.TInput
import org.scalatest._
import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test

/**
  * Created by jiaji on 2016-10-01.
  */
class NABMatcherTest extends TestNGSuite with ShouldMatchers {

  val input:TInput = fromAtomArrayEng(IndexedSeq(
    textAtom("Now"),
    FBI,
    textAtom("and"),
    Microsoft,
    textAtom("announce")
  ))

  val input1:TInput = fromAtomArrayEng(IndexedSeq(
    textAtom("Now"),
    FBI,
    textAtom("and"),
    textAtom("x"),
    Microsoft,
    textAtom("announce")
  ))

  val input2:TInput = fromAtomArrayEng(IndexedSeq(
    textAtom("Now"),
    FBI,
    textAtom("x"),
    textAtom("and"),
    Microsoft,
    textAtom("announce")
  ))

  implicit val checkerLib = StaticSubMatchCheckerLib
  val orgCompanyMatcherId = "OrgCompany"
  val entityMatcher = fromAtomMatcher(E(EmptyRegexDict, Array("Company", "Organization")), EmptyCheckerIds, Option(orgCompanyMatcherId))
  val andMatcher = fromAtomMatcher(FExact("and"))

  @Test
  def testNAB = {
    val mm = MatcherManager.create

    val nabMatcherId = "nab"
    val nabMatcher = matchersNAB(andMatcher, entityMatcher, List(ListNGramId), false, Option(nabMatcherId))
    val anbMatcherId = "anb"
    val anbMatcher = matchersNAB(andMatcher, entityMatcher, List(ListNGramId), true, Option(anbMatcherId))
    mm.add(entityMatcher)
    mm.add(andMatcher)
    mm.add(nabMatcher)
    mm.add(anbMatcher)

    val resultPool = mm.m(input, StaticSubMatchCheckerLib, MatcherManager.EmptyMIdFilters)
    val orgCompanies = resultPool.query(orgCompanyMatcherId)
    orgCompanies.size shouldBe(2)

    val nabMatches = resultPool.query(nabMatcherId)
    nabMatches.size shouldBe(1)
    val anbMatches = resultPool.query(anbMatcherId)
    anbMatches.size shouldBe(1)
    val resultPool1 = mm.m(input1, StaticSubMatchCheckerLib, MatcherManager.EmptyMIdFilters)
    val nabMatches1 = resultPool1.query(nabMatcherId)
    nabMatches1.size shouldBe(2)
    val resultPool2 = mm.m(input2, StaticSubMatchCheckerLib, MatcherManager.EmptyMIdFilters)
    val anbMatches2 = resultPool2.query(anbMatcherId)
    anbMatches2.size shouldBe(2)


  }

  @Test
  def testLookaround = {
    val mm = MatcherManager.create

    val eabMatcherId = "eab"
    val eabMatcher = matchersLookaround(andMatcher, entityMatcher, List(ListNGramId), false, Option(eabMatcherId))
    val aebMatcherId = "aeb"
    val aebMatcher = matchersLookaround(andMatcher, entityMatcher, List(ListNGramId), true, Option(aebMatcherId))
    mm.add(entityMatcher)
    mm.add(andMatcher)
    mm.add(eabMatcher)
    mm.add(aebMatcher)

    val resultPool = mm.m(input, StaticSubMatchCheckerLib, MatcherManager.EmptyMIdFilters)
    val orgCompanies = resultPool.query(orgCompanyMatcherId)
    orgCompanies.size shouldBe(2)

    val eabMatches = resultPool.query(eabMatcherId)
    eabMatches.size shouldBe(1)
    val aebMatches = resultPool.query(aebMatcherId)
    aebMatches.size shouldBe(1)

    val resultPool1 = mm.m(input1, StaticSubMatchCheckerLib, MatcherManager.EmptyMIdFilters)
    val eabMatches1 = resultPool1.query(eabMatcherId)
    eabMatches1.size shouldBe(0)

    val resultPool2 = mm.m(input2, StaticSubMatchCheckerLib, MatcherManager.EmptyMIdFilters)
    val aebMatches2 = resultPool2.query(aebMatcherId)
    aebMatches2.size shouldBe(0)


  }
}
