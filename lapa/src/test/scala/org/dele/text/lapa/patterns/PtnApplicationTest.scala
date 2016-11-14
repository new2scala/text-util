package org.dele.text.lapa.patterns

import org.dele.text.maen.{AtomPropMatcherLib, TInput}
//import org.dele.text.maen.TestHelper._
import org.dele.text.maen.matchers.{TMatcher, MatcherManager}
import org.scalatest.ShouldMatchers
import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test

/**
  * Created by jiaji on 2016-02-12.
  */
class PtnApplicationTest extends TestNGSuite with ShouldMatchers {
  import PtnUtil._
  import org.dele.text.lapa.TestHelper._
  import org.dele.text.maen.test.TestAtom._
  import org.dele.text.maen.test.TestInput._
  import TMatcher._
  import AtomPropMatcherLib._

  @Test
  def t1 = {

/*
    var appliedMatchers = applyPtnGroup(patternGroup1, testPatternAppEng)
    appliedMatchers.size shouldBe(1)
    appliedMatchers = applyPtnGroup(patternGroup1, testPatternAppZhs)
    appliedMatchers.size shouldBe(3)
    appliedMatchers = applyPtnGroup(patternGroup2, testPattern2AppEng)
    appliedMatchers.size shouldBe(2)

    val grp1 = applyPtnGroup(patternGroup1, testPatternAppEng)
    val grp2 = applyPtnGroup(patternGroup2, testPattern2AppEng)
    val grp3 = applyPtnGroup(patternGroup3, testPattern3AppEng)
    val grp4 = applyPtnGroup(patternGroup4, testPattern4AppEng)

    val mm = MatcherManager.create
    grp1.foreach(mm.add(_))
    grp2.foreach(mm.add(_))
    grp3.foreach(mm.add(_))
    grp4.foreach(mm.add(_))
    mm.add(fromAtomMatcher(E(Array("Company", "Organization")), Option("org-list")))
    mm.add(fromAtomMatcher(E(Array("Country")), Option("country-list")))
    mm.add(fromAtomMatcher(F("attack"), Option("attack-words")))
    mm.add(fromAtomMatcher(F("against"), Option("against-words")))
    mm.add(fromAtomMatcher(F("launch"), Option("launch-words")))

    val input:TInput = IndexedSeq(
      textAtom("Now"),
      entityAtom("Anonymous", "Organization", "OrgEntity"),
      textAtom("launch"),
      textAtom("attack"),
      textAtom("against"),
      entityAtom("FBI", "Organization", "OrgEntity"),
      textAtom("and"),
      entityAtom("Microsoft", "Company", "OrgEntity")
    )

    val resultPool = mm.m(input)
    resultPool.query(testPatternId).size shouldBe(2)

    val zhsgrp1 = applyPtnGroup(patternGroup1, testPatternAppZhs)
    val zhsgrp2 = applyPtnGroup(patternGroup2, testPattern2AppZhs)
    val zhsgrp3 = applyPtnGroup(patternGroup3, testPattern3AppZhs)
    val zhsgrp4 = applyPtnGroup(patternGroup4, testPattern4AppZhs)

    val zhsmm = MatcherManager.create
    zhsgrp1.foreach(zhsmm.add(_))
    zhsgrp2.foreach(zhsmm.add(_))
    zhsgrp3.foreach(zhsmm.add(_))
    zhsgrp4.foreach(zhsmm.add(_))
    zhsmm.add(fromAtomMatcher(E(Array("Company", "Organization")), Option("org-list")))
    zhsmm.add(fromAtomMatcher(E(Array("Country")), Option("country-list")))
    zhsmm.add(fromAtomMatcher(F("攻击"), Option("attack-words")))
    zhsmm.add(fromAtomMatcher(F("针对"), Option("against-words")))
    zhsmm.add(fromAtomMatcher(F("发起"), Option("launch-words")))

    val zhsinput:TInput = IndexedSeq(
      textAtom("最近"),
      entityAtom("匿名者", "Organization", "OrgEntity"),
      textAtom("发起"),
      textAtom("针对"),
      entityAtom("FBI", "Organization", "OrgEntity"),
      textAtom("和"),
      entityAtom("Microsoft", "Company", "OrgEntity"),
      textAtom("攻击")
    )

    val zhsResultPool = zhsmm.m(zhsinput)
    zhsResultPool.query(testPatternId).size shouldBe >(2)
    */

  }
}
