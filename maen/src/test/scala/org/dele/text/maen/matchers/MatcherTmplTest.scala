package org.dele.text.maen.matchers

import org.dele.text.maen.ConfValueStringParser.Parsed
import org.dele.text.maen.TestHelper._
import SubMatchCheckerLib._
import org.dele.text.maen.test.TestAtom._
import org.dele.text.maen.AtomPropMatcherLib
import org.dele.text.maen.test.{TestAtom, TestInput}
import org.dele.text.maen.TInput
import org.scalatest.ShouldMatchers
import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test

/**
  * Created by jiaji on 2016-02-29.
  */
class MatcherTmplTest extends TestNGSuite with ShouldMatchers {
  @Test
  def t1 = {
    import MatcherTmpl._
    import TSubMatchChecker._
    import TMatcher._
    import org.dele.text.maen.AtomPropMatcherLib._
    import org.dele.text.maen.test.TestInput._
    val lngChecker = "lngChecker"
    implicit val checkerLib = new SubMatchCheckerLib(Map(lngChecker -> Parsed("Lng", Array())), List()) //SubMatchCheckerLib.c(Map(lngChecker -> ListNGramChecker))
    val tmplId = "id1"
    val tlib = new MatcherTmplLib(
      List(new MatcherTmplDef(tmplId, "MTL_RepetitionAll", lngChecker)), List()
    )
    val matcherId = "orgCompanyMatcher"
    val matcher = fromAtomMatcher(E(EmptyRegexDict, Array("Organization", "Company")), EmptyCheckerIds, Option(matcherId))
    val mm = MatcherManager.create
    mm.add(matcher)

    val id2 = "orgCompanySeq"
    val m2 = tlib.spawn(tmplId, Array(Array(matcherId)), EmptyRegexDict, Option(id2))
    mm.add(m2)

    val input:TInput = fromAtomArrayEng(IndexedSeq(
      textAtom("Now"),
      entityAtom("FBI", "Organization", "OrgEntity"),
      entityAtom("Microsoft", "Company", "OrgEntity"),
      textAtom("and"),
      entityAtom("IBM", "Company", "OrgEntity")
    ))

    val rp = mm.m(input, EmptySubMatchCheckerLib, MatcherManager.EmptyMIdFilters)
    val r = rp.query(id2)
    r.size shouldBe >(0)
  }
}
