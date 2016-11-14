package org.dele.text.maen.matchers

import org.dele.text.maen.TestHelper._
import SubMatchCheckerLib._
import org.scalatest.ShouldMatchers
import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test

/**
  * Created by jiaji on 2016-02-10.
  */
class DepMapTest extends TestNGSuite with ShouldMatchers {
  import org.dele.text.maen.AtomPropMatcherLib._
  import TMatcher._
  val idWord = "atom-matcher-f-word"
  val idPhrase = "atom-matcher-f-phrase"
  val idSentence = "atom-matcher-f-sentence"
  val idGroup = "atom-matchers"
  val idGroup2 = "atom-matchers2"

  implicit val smlib = EmptySubMatchCheckerLib

  @Test
  def t1 = {
    val matchers = Array(
      fromAtomMatcher(FExact("word"),  EmptyCheckerIds, Option(idWord)),
      fromAtomMatcher(FExact("phrase"), EmptyCheckerIds, Option(idPhrase))
    )
    val compMatcher = matchersOR(idGroup, matchers)

    val matchers2 = Array(
      fromAtomMatcher(FExact("word"), EmptyCheckerIds, Option(idWord)),
      fromAtomMatcher(FExact("sentence"), EmptyCheckerIds, Option(idSentence))
    )
    val compMatcher2 = matchersOR(idGroup2, matchers2)

    val depMap = DepMap.create
    depMap += compMatcher
    depMap += compMatcher2
    depMap.getMatcherIdsDepOn(idWord) shouldBe(Set(idGroup, idGroup2))
    depMap.getMatcherIdsDepOn(idPhrase) shouldBe(Set(idGroup))
    depMap.getMatcherIdsDepOn(idSentence) shouldBe(Set(idGroup2))
  }

  import DepMap._
  @Test
  def testFindCircles = {
    val deps = Map(
      "list1" -> Set("list11", "list12"),
      "list11" -> Set("list111", "list112"),
      "list112" -> Set("list1", "list1121")
    )

    val circle1 = IndexedSeq("list1", "list11", "list112", "list1")

    val c = findDepCircles("list1", deps)
    c shouldBe Set(circle1)

    val deps1 = Map(
      "list1" -> Set("list11", "list12"),
      "list2" -> Set("list21", "list112"),
      "list11" -> Set("list111", "list112"),
      "list112" -> Set("list1", "list1121", "list1122"),
      "list1122" -> Set("list11221", "list11222"),
      "list11221" -> Set("list112", "list112211")
    )
    val circle2 = IndexedSeq("list112", "list1122", "list11221", "list112")

    val c1 = findDepCircles("list1", deps1)
    c1 shouldBe Set(circle1, circle2)

    val all = computeAllCircles(deps1)
    //val merged = mergeCircles(all)
    all shouldBe Set(circle1.toSet ++ circle2)
  }
}
