package org.dele.text.maen

import org.dele.text.maen.test.TestAtom.Atom
import org.dele.text.maen.matchers.TMatcher
import org.scalatest.ShouldMatchers
import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test

/**
  * Created by jiaji on 2016-02-09.
  */
class FromAtomMatcherTest extends TestNGSuite with ShouldMatchers {
  import TMatcher._
  import TestHelper._
  import AtomPropMatcherLib._
  import org.dele.text.maen.test.TestInput._
  @Test
  def t1 = {
    implicit val subMatchCheckerLib = EmptySubMatchCheckerLib
    val seqMatcher = fromAtomMatcher(F(EmptyRegexDict, "word"))
    val input = fromAtomArrayEng(IndexedSeq(
      Atom("Word", Map()),
      Atom("to", Map()),
      Atom("word", Map())
    ))

    val matches = seqMatcher.m(
      DummyResultPool(input)
    )

    matches.size shouldBe(2)

  }

}
