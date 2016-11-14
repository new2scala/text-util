package org.dele.text.maen.matchers

import org.dele.text.maen.ConfValueStringParser.Parsed
import org.dele.text.maen.matchers.StoppedByMatcherManager.StoppedByConfig
import org.dele.text.maen.matchers.SubMatchCheckerLib._
import org.dele.text.maen.test.TestAtom._
import org.dele.text.maen.{AtomPropMatcherLib, TAtom, TInput, TestHelper}
import org.dele.text.maen.{TAtom, TInput}
import org.scalatest.ShouldMatchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test

/**
  * Created by jiaji on 2016-02-18.
  */
class RepeatableMatcherTest extends TestNGSuite with ShouldMatchers with TableDrivenPropertyChecks {
  import TMatcher._
  import org.dele.text.maen.AtomPropMatcherLib._
  import TSubMatchChecker._
  import org.dele.text.maen.TestHelper._

  import org.dele.text.maen.test.TestInput._

  val checkerId = "cid"
  val listConnectorId = "list-connector"
  implicit val checkerLib = new SubMatchCheckerLib(
    Map(checkerId -> Parsed("All", Array(Array(listConnectorId)))), List()
  )

  def createInput(atoms:Seq[TAtom]):TInput = {
    fromAtomArrayEng(IndexedSeq(
      textAtom("Now"),
      Anonymous
    ) ++ atoms)
  }

  val listConnector = matchersOR(
    listConnectorId,
    Seq(
      fromAtomMatcher(FExact(",")),
      fromAtomMatcher(FExact("and"))
    )
  )

  val OrgMId = "Org"
  val OrgMatcher = fromAtomMatcher(OrganizationEntity, EmptyCheckerIds, Option(OrgMId))(checkerLib)
  val CmpMId = "Company"
  val CmpMatcher = fromAtomMatcher(CompanyEntity, EmptyCheckerIds, Option(CmpMId))(checkerLib)
  val entityList = repeatMatcher(
    Seq(
      queryPoolMatcher(Set(OrgMId, CmpMId))
    ),
    false,
    Option("cmp-org-list"),
    List(checkerId)
  )

  val testData = Table(
    ("testAtoms", "matchRanges"),
    (
      Seq(FBI, _comma, Microsoft),
      Set(1 to 1, 2 to 2, 4 to 4, 2 to 4)
      ),
    (
      Seq(FBI, _and, Microsoft),
      Set(1 to 1, 2 to 2, 4 to 4, 2 to 4)
      ),
    (
      Seq(_comma, FBI, _and, Microsoft),
      Set(1 to 5, 1 to 1, 3 to 3, 5 to 5, 1 to 3, 3 to 5)
      ),
    (
      Seq(_comma, FBI, _and, _comma, Microsoft),
      Set(1 to 3, 1 to 1, 3 to 3, 6 to 6)
      ),
    (
      Seq(FBI, Microsoft),
      Set(1 to 1, 2 to 2, 3 to 3)
      )
  )

  @Test
  def t1 = {
    val mm = MatcherManager.create
    mm.add(listConnector)
    mm.add(CmpMatcher)
    mm.add(OrgMatcher)
    mm.add(entityList)
    forAll(testData) {
      (testAtoms, resultRanges) => {
        val input = createInput(testAtoms)
        val resultPool = mm.m(input, checkerLib, MatcherManager.EmptyMIdFilters)
        val entities = resultPool.query(entityList.id.get)
        entities.size shouldBe(resultRanges.size)
        resultRanges.foreach(r => entities.exists(_.range == r) shouldBe true)
      }
    }
  }

  val _announce = fromAtomMatcher(FExact("announce"))
  val notOrgMId = Option("not-org")
  val notOrgMatcher = matchersOrderedAllPositive(Seq(queryPoolMatcher(Set(OrgMId, CmpMId)), _announce), List(ListNGramId), notOrgMId)(StaticSubMatchCheckerLib)
  val stoppedByMgr = new StoppedByMatcherManager(Map(
    OrgMId -> List(StoppedByConfig(notOrgMId.get, true)),
    CmpMId -> List(StoppedByConfig(notOrgMId.get, true))
  ))

  def createInput2(atoms:Seq[TAtom]):TInput = fromAtomArrayEng(IndexedSeq(
      textAtom("Now"),
      Anonymous
    ) ++ atoms ++ Seq(textAtom("announce")))


  val testData2 = Table(
    ("testAtoms", "matchRanges"),
    (
      Seq(FBI, _comma, Microsoft),
      Set(1 to 1, 2 to 2)
      ),
    (
      Seq(FBI, _and, Microsoft),
      Set(1 to 1, 2 to 2)
      ),
    (
      Seq(_comma, FBI, _and, Microsoft),
      Set(1 to 3, 1 to 1, 3 to 3)
      ),
    (
      Seq(_comma, FBI, _and, _comma, Microsoft),
      Set(1 to 3, 1 to 1, 3 to 3)
      ),
    (
      Seq(FBI, Microsoft),
      Set(1 to 1, 2 to 2)
      )
  )


  @Test
  def t2 = {
    implicit val sbmm = stoppedByMgr
    val mm = MatcherManager.create
    mm.add(listConnector)
    mm.add(CmpMatcher)
    mm.add(OrgMatcher)
    mm.add(entityList)
    mm.add(notOrgMatcher)
    mm.setStoppedByManager(stoppedByMgr)
    forAll(testData2) {
      (testAtoms, resultRanges) => {
        val input = createInput2(testAtoms)
        val resultPool = mm.m(input, checkerLib, MatcherManager.EmptyMIdFilters)
        val entities = resultPool.query(entityList.id.get)
        entities.size shouldBe resultRanges.size
        resultRanges.foreach(r => entities.exists(_.range == r) shouldBe true)
      }
    }
  }

}
