package org.dele.text.lapa.patterns

import LangPatternGroupTemplate.LangPatternGroupTemplateLib
import org.scalatest._
import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test

/**
  * Created by jiaji on 2016-08-19.
  */
class PatternGroupTemplateTest extends TestNGSuite with ShouldMatchers {
  val templateJson =
    """
      |[
      | {
      |  "name": "A+in+B",
      |  "templates": [
      |   {
      |    "languages": [ "eng" ],
      |    "patterns": [
      |     {
      |      "nodes": [
      |       { "content": "$1" },
      |       { "content": "in" },
      |       { "content": "$2" }
      |      ]
      |     },
      |     {
      |      "nodes": [
      |       { "content": "A_B($1|$2)" }
      |      ]
      |     }
      |    ]
      |   },
      |   {
      |    "languages": [ "zhs" ],
      |    "patterns": [
      |     {
      |      "nodes": [
      |       { "content": "A_B($1|$2) A_'s_B($1|$2)" }
      |      ]
      |     }
      |    ]
      |   }
      |  ]
      | }
      |]
    """.stripMargin
  @Test
  def t1 = {
    val templateSet = LangPatternGroupTemplate.parseJson(templateJson)
    val lib = new LangPatternGroupTemplateLib(templateSet)
    lib shouldNot be(null)
    val ptnTmplEng = lib.byNameAndLang("A+in+B", "eng").get
    val ptnGrpEng1 = ptnTmplEng.instantiate("id", Array(Array("param1"), Array("param2")), List(), Option(false))
    ptnGrpEng1 shouldNot be(null)
    val ptnGrpEng2 = ptnTmplEng.instantiate("id", Array(Array("param1"), Array("param21", "param22")), List(), Option(false))
    ptnGrpEng2 shouldNot be(null)
    val ptnGrpZhs = lib.byNameAndLang("A+in+B", "zhs").get.instantiate("id", Array(Array("param3"), Array("param4")), List(), Option(false))
    ptnGrpZhs shouldNot be(null)
  }
}
