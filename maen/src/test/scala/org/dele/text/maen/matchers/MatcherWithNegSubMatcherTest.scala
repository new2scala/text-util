package org.dele.text.maen.matchers

import org.dele.text.maen.AtomPropMatcherLib._
import org.dele.text.maen.TestHelper._
import SubMatchCheckerLib._
import TMatcher._
import org.dele.text.maen.TInput
import org.dele.text.maen.test.TestAtom._
import org.scalatest.ShouldMatchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test

/**
  * Created by jiaji on 2016-04-21.
  */
class MatcherWithNegSubMatcherTest extends TestNGSuite with ShouldMatchers with TableDrivenPropertyChecks {
  import org.dele.text.maen.test.TestInput._

  implicit val smclib = EmptySubMatchCheckerLib
  val orgCompanyMatcherId = "org_company"
  val entityMatcher = fromAtomMatcher(E(EmptyRegexDict, Array("Company", "Organization")), EmptyCheckerIds, Option(orgCompanyMatcherId))
  val andMatcher = fromAtomMatcher(FExact("and"))
  val matcherId = Option("org_company_list")
  val _now = textAtom("Now")
  val now_fbi_and_ms = IndexedSeq(
    _now,
    FBI,
    _and,
    Microsoft
  )
  val now_fbi_and_cia_ms = IndexedSeq(
    _now,
    FBI,
    _and,
    CIA,
    Microsoft
  )

  val now_fbi_and = IndexedSeq(
    _now,
    FBI,
    _and
  )

  val now_fbi_cia_and_ms = IndexedSeq(
    _now,
    FBI,
    CIA,
    _and,
    Microsoft
  )
  val now_fbi_cia_and = IndexedSeq(
    _now,
    FBI,
    CIA,
    _and
  )

  val mseq1 = Seq(entityMatcher, entityMatcher, andMatcher, entityMatcher)
  val negIdx1 = IndexedSeq(1)
  val negIdx0 = IndexedSeq(0)
  val negIdx3 = IndexedSeq(3)
  val negIdx13 = IndexedSeq(1,3)
  val negIdx03 = IndexedSeq(0,3)
  val testData = Table(
    ("matcherSeq", "negSeq", "input", "matchRanges"),
    (
      mseq1,
      negIdx1,
      now_fbi_and_ms,
      Set(1 to 3)
    ),
    (
      mseq1,
      negIdx1,
      now_fbi_cia_and_ms,
      Set(2 to 4)
    ),
    (
      mseq1,
      negIdx1,
      now_fbi_and_cia_ms,
      Set(1 to 3, 1 to 4)
    ),
    (
      mseq1,
      negIdx0,
      now_fbi_and_ms,
      Set(1 to 3)
    ),
    (
      mseq1,
      negIdx0,
      now_fbi_cia_and_ms,
      Set(1 to 4)
    ),
    (
      mseq1,
      negIdx0,
      now_fbi_and_ms,
      Set(1 to 3)
    ),
    (
      mseq1,
      negIdx13,
      now_fbi_cia_and_ms,
      Set()
    ),
    (
      mseq1,
      negIdx13,
      now_fbi_and_ms,
      Set()
    ),
    (
      mseq1,
      negIdx03,
      now_fbi_cia_and,
      Set(1 to 3)
    ),
    (
      mseq1,
      negIdx13,
      now_fbi_and,
      Set(1 to 2)
    ),
    (
      mseq1,
      negIdx3,
      now_fbi_cia_and_ms,
      Set()
    ),
    (
      mseq1,
      negIdx3,
      now_fbi_and_ms,
      Set()
    ),
    (
      mseq1,
      negIdx3,
      now_fbi_cia_and,
      Set(1 to 3)
    ),
    (
      mseq1,
      negIdx3,
      now_fbi_and,
      Set()
    )

  )

  import SubMatchCheckerLib._

  @Test
  def t1 = {
    forAll(testData) {
      (matcherSeq, negSeq, input, matchRanges) => {
        val mm = MatcherManager.create
        val matchers = matchersOrdered(matcherSeq, negSeq, EmptyCheckerIds, matcherId)

        mm.add(entityMatcher)
        mm.add(matchers)

        val resultPool = mm.m(fromAtomArrayEng(input), EmptySubMatchCheckerLib, MatcherManager.EmptyMIdFilters)

        val resultList = resultPool.query(matcherId.get)
        //println(s"matches: ${resultList.size}")
        resultList.size shouldBe matchRanges.size
        resultList.map(_.range) shouldBe matchRanges
      }
    }
  }

}
