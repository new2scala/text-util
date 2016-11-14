package org.dele.text.maen.matchers

import org.dele.text.maen.test.TestAtom._
import org.dele.text.maen.{AtomPropMatcherLib, TInput}
import org.dele.text.maen.TInput
import org.scalatest.ShouldMatchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test

/**
  * Created by jiaji on 2016-02-16.
  */
class TSubMatchCheckerTest extends TestNGSuite with ShouldMatchers with TableDrivenPropertyChecks {
  import TSubMatchChecker._
  import org.dele.text.maen.AtomPropMatcherLib._
  import org.dele.text.maen.test.TestInput._
  import TMatcher._
  import org.dele.text.maen.TestHelper._

  val input:TInput = fromAtomArrayEng(IndexedSeq(
    textAtom("Now"),
    Anonymous,
    textAtom("launch"),
    textAtom("attack"),
    textAtom(","),
    FBI
  ))

  implicit val smcLib = EmptySubMatchCheckerLib

  import SubMatchCheckerLib._
  val orgMatcher = E(EmptyRegexDict, Array("Organization"))
  val attackMatcher = FExact("attack")
  val launchMatcher = FExact("launch")
  val separatorAtomMatcher = F(EmptyRegexDict, ",",";")
  val separatorMatcherId = "id_separator"
  val separatorMatcher = fromAtomMatcher(separatorAtomMatcher, EmptyCheckerIds, Option(separatorMatcherId))
  val randomMatcherId = "id_random"
  val randomMatcher = fromAtomMatcher(FExact("aslkezxlcjv"), EmptyCheckerIds, Option(randomMatcherId))
  val testAtomCheckerData = Table(
    ("orderedMatcher", "matchCheckerId", "hasMatch", "matchRange"),
    (
      Seq(
        fromAtomMatcher(orgMatcher),
        fromAtomMatcher(attackMatcher),
        fromAtomMatcher(orgMatcher)
      ),
      NoCheckId,
      true,
      Option(1 to 5)
    ),
    (
      Seq(
        fromAtomMatcher(orgMatcher),
        fromAtomMatcher(launchMatcher),
        fromAtomMatcher(orgMatcher)
      ),
      ListNGramId,
      false,
      None
    ),
    (
      Seq(
        fromAtomMatcher(orgMatcher),
        fromAtomMatcher(launchMatcher),
        fromAtomMatcher(attackMatcher)
      ),
      ListNGramId,
      true,
      Option(1 to 3)
    )
  )

  @Test
  def testAtomMatcherChecker = {

    forAll(testAtomCheckerData) {
      (orderedMatcher, matchCheckerId, hasMatch, matchRange) => {
        val matcher = matchersOrderedAllPositive(orderedMatcher, List(matchCheckerId), Option("id"))
        val matches = matcher.m(DummyResultPool(input))
        val found = matches.size > 0
        found shouldBe(hasMatch)
        if (found) {
          matches.toList(0).range shouldBe(matchRange.get)
        }
      }
    }
  }

  val testCheckerData = Table(
    ("orderedMatcher", "hasMatch"),
    (
      Seq(
        fromAtomMatcher(orgMatcher),
        fromAtomMatcher(attackMatcher),
        fromAtomMatcher(orgMatcher)
      ),
      false
      ),
    (
      Seq(
        fromAtomMatcher(attackMatcher),
        fromAtomMatcher(FExact("FBI"))
      ),
      false
      ),
    (
      Seq(
        fromAtomMatcher(orgMatcher),
        fromAtomMatcher(launchMatcher),
        fromAtomMatcher(attackMatcher)
      ),
      true
      )
  )
/*
  @Test
  def testMatcherChecker = {
    forAll(testCheckerData) {
      (orderedMatcher, hasMatch) => {
        val mm = MatcherManager.create

        val mid = "id"

        val matcher = matchersOrdered(mid,
          orderedMatcher,
          matchesInBetween_Not(Set(separatorMatcherId, randomMatcherId))
        )
        mm.add(matcher)
        mm.add(separatorMatcher)
        mm.add(randomMatcher)

        val resultPool = mm.m(input)
        val r = resultPool.query(mid)
        val found = r.size > 0
        found shouldBe(hasMatch)
      }
    }
  }
  */
}
