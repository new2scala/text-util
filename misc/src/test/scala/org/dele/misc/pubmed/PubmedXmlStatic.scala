package org.dele.misc.pubmed

import scala.xml.{Elem, NodeSeq}

/**
  * Created by dele on 2017-03-06.
  */
object PubmedXmlStatic {

  case class NodeProcData(label:String, attrs: Array[String], elems: Array[String]) {
    def checkNode(n: Elem) = PubmedXmlHelpers.nodeCheck(n, attrs, elems)
  }
  /*
  val NodeProdDataPool = Map(
    "Author" -> NodeProcData(
      "Author",
      List("ValidYN"),
      List("LastName", "ForeName", "Initials", "Suffix", "Identifier", "AffiliationInfo")
      )
  )
  */
}
