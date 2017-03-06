package org.dele.misc.pubmed

/**
  * Created by dele on 2017-03-06.
  */
object PubmedXmlStatic {

  case class NodeProcData(n:String, attrs: Array[String], elems: Array[String])
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
