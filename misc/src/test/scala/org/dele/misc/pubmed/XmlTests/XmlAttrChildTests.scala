package org.dele.misc.pubmed.XmlTests

import org.dele.misc.pubmed.PubmedXmlHelpers
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
      val foundChildren = PubmedXmlHelpers.childElems(root).map(_.label)

      foundChildren shouldBe children
    }
  }

  val emtpyElemArr = Array[String]()
  val nodeCheckTestData = Table(
    ("xmlStr", "handledAttrs", "handledElems", "unhandled"),
    (singleAttr, Array("attr"), emtpyElemArr, (Set[String](), Set("c1"))),
    (multiAttr, Array("attr1"), emtpyElemArr, (Set("attr2"), Set[String]())),
    (multiAttrChildren, Array("attr1"), Array("c1"), (Set("attr2"), Set("c2")))
  )

  import org.dele.misc.pubmed.PubmedXmlHelpers._
  "nodeCheck tests" should "pass" in {
    forAll(nodeCheckTestData) { (xmlStr, handledAttrs, handledElems, unhandled) =>
      val root = xml.XML.loadString(xmlStr)
      val ua = nodeCheck(root, handledAttrs, handledElems)
      ua shouldBe unhandled
    }


  }
}
