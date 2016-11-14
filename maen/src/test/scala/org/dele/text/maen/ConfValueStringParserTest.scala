package org.dele.text.maen

import org.scalatest.ShouldMatchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test

/**
  * Created by jiaji on 2016-02-16.
  */
class ConfValueStringParserTest extends TestNGSuite with ShouldMatchers with TableDrivenPropertyChecks {
  import ConfValueStringParser._

  val parseTmplTestData = Table(
    ("matcherDef", "expectedTmplId", "expectedParams"),
    ("E(OrgEntity)", "E", "OrgEntity"),
    ("E(OrgEntity|GeoEntity)", "E", "OrgEntity|GeoEntity"),
    ("F(word)", "F", "word"),
    ("F(()", "F", "("),
    ("F(word1|word2)", "F", "word1|word2"),
    ("L(word1|word2)", "L", "word1|word2"),
    ("EA(category,ThreatActor)", "EA", "category,ThreatActor"),
    ("EA(category, Government|Law)", "EA", "category, Government|Law")
  )
  @Test
  def testParseTmpl = {
    forAll(parseTmplTestData) {
      (matcherDef, expectedTmplId, expectedParams) => {
        val p = parseTmplId(matcherDef)
        p._1 shouldBe(expectedTmplId)
        p._2.get shouldBe(expectedParams)
      }
    }
  }

  val parseParamTestData = Table(
    ("paramStr", "expected"),
    ("w1", Array(Array("w1"))),
    (" w1", Array(Array("w1"))),
    ("w1/w2", Array(Array("w1", "w2"))),
    ("w1 / w2", Array(Array("w1", "w2"))),
    (" p1 / p2 | s1 / s2 ", Array(Array("p1", "p2"), Array("s1", "s2")))
  )
  @Test
  def testParseParams = {
    forAll(parseParamTestData) {
      (paramStr, expected) => {
        val para = parseParams(paramStr)
        para shouldBe(expected)
      }
    }
  }

  val regexTestData = Table(
    ("str", "expected"),
    ("a\\|b", Array("a\\|b")),
    ("a|b", Array("a", "b"))
  )

  @Test
  def regexTests = {
    forAll(regexTestData) {
      (str, expected) => {
        val m = FirstSeparator.split(str)
        m shouldBe(expected)
      }
    }
  }

  val parseSpecialTestData = Table(
    ("paramStr", "expected"),
    ("""\|""", Array(Array("|"))),
    ("""\\""", Array(Array("\\"))),
    ("""\|/\//\\""", Array(Array("|", "/", "\\"))),
    ("""\| / \/ / \\""", Array(Array("|", "/", "\\")))
  )

  @Test
  def testEscapeChar = {
    forAll(parseSpecialTestData) {
      (paramStr, expected) => {
        val para = parseParams(paramStr)
        para shouldBe(expected)
      }
    }
  }

}
