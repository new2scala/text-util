package org.dele.misc.pubmed.XmlTests

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

import scala.xml.Elem

/**
  * Created by dele on 2017-03-04.
  */
class XmlAttrChildTests extends FlatSpec with Matchers with TableDrivenPropertyChecks {

  val singleAttr =
    """
      |<root attr="attrval">
      | <c1>child node</c1>
      |</root>
    """.stripMargin

  val multiAttr =
    """
      |<root attr1="attr1val" attr2="attr2val">
      |</root>
    """.stripMargin

  val multiAttrChildren =
    """
      |<root attr1="attr1val" attr2="attr2val">
      | <c1 c1attr="c1a">c1 test</c1>
      | <c2 c2attr="c2a">c2 test</c2>
      |</root>
    """.stripMargin

  val testData = Table(
    ("xmlStr", "attrs", "children"),
    (singleAttr, Map("attr" -> "attrval"), List("c1")),
    (multiAttr, Map("attr1" -> "attr1val", "attr2" -> "attr2val"), List()),
    (multiAttrChildren, Map("attr1" -> "attr1val", "attr2" -> "attr2val"), List("c1", "c2"))
  )

  "Attrs/children tests" should "pass" in {
    forAll(testData) { (xmlStr, attrs, children) =>
      val root = xml.XML.loadString(xmlStr)

      val foundAttrs = root.attributes.map(attr => attr.key -> attr.value.text).toMap
      foundAttrs shouldBe attrs
      val foundChildren = root.child.flatMap{
        case elem:Elem => Option(elem.label)
        case _ => None
      }.toList

      foundChildren shouldBe children
    }
  }

  val nodeCheckTestData = Table(
    ("xmlStr", "handledAttrs", "unhandledAttrs"),
    (singleAttr, Array("attr"), Set[String]()),
    (multiAttr, Array("attr1"), Set("attr2"))
  )

  import org.dele.misc.pubmed.PubmedXmlHelpers._
  "nodeCheck tests" should "pass" in {
    forAll(nodeCheckTestData) { (xmlStr, handledAttrs, unhandledAttrs) =>
      val root = xml.XML.loadString(xmlStr)
      val ua = nodeCheck(root, handledAttrs, Array())
      ua shouldBe unhandledAttrs
    }


  }
}
