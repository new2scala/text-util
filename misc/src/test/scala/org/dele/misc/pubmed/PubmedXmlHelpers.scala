package org.dele.misc.pubmed

import org.dele.misc.pubmed.PubmedXmlStatic.NodeProcData

import scala.xml.{Elem, NodeSeq}

/**
  * Created by dele on 2017-02-25.
  */
object PubmedXmlHelpers {
  def node2Str(n:NodeSeq):Option[String] = if (n.isEmpty) None else Option(n.text)

  def nodeAttrKeys(n:Elem):Set[String] = n.attributes.map(_.key).toSet
  def childElems(elem:Elem):Seq[Elem] = elem.child.flatMap{
    case elem:Elem => Option(elem)
    case _ => None
  }

  def nodeCheck(elem:Elem, attrs:Array[String], elems:Array[String]):(Set[String], Set[String]) = {
    //val elem = n.asInstanceOf[Elem]
    val rem = nodeAttrKeys(elem) -- attrs
    if (rem.nonEmpty) {
      val unhandledAttrs = rem.mkString(",")
      println(s"In label <${elem.label}> attributes [$unhandledAttrs] not handled")
    }
    val remElems = childElems(elem).map(_.label).toSet -- elems
    if (remElems.nonEmpty) {
      val unhandledElems = remElems.mkString(",")
      println(s"In label <${elem.label}> elements [$unhandledElems] not handled")
    }

    (rem, remElems)
  }

  def xml2Identifier(idNode: NodeSeq): _Identifier = {
    val source = idNode \ "@Source"

    _Identifier(node2Str(source), idNode.text)
  }

  val authorPD = NodeProcData(
    "Author", Array("ValidYN"),
    Array("LastName", "ForeName", "Initials", "Suffix", "Identifier", "AffiliationInfo", "CollectiveName")
  )
  def xml2Author(author: NodeSeq):_Author = {
    nodeCheck(author.asInstanceOf[Elem], authorPD.attrs, authorPD.elems)
    val affiNodes = author \ "AffiliationInfo"
    val affis = affiNodes.map{ n =>
      val aff = (n \ "Affiliation").text
      _AffiliationInfo(aff)
    }.toList

    val idNode = author \ "Identifier"

    _Author(
      ValidYN = node2Str(author \ "@ValidYN"),
      LastName = node2Str(author \ "LastName"),
      ForeName = node2Str(author \ "ForeName"),
      Suffix = node2Str(author \ "Suffix"),
      Initials = node2Str(author \ "Initials"),
      if (idNode.isEmpty) None else Option(xml2Identifier(idNode)),
      AffiliationInfo = affis,
      CollectiveName = node2Str(author \ "CollectiveName")
    )
  }

  def xml2AuthorList(authorList: NodeSeq) = {
    val authorNodes = authorList \ "Author"
    val authors = authorNodes.map(xml2Author).toList
    _AuthorList(
      node2Str(authorList \ "@CompleteYN"),
      authors
    )
  }

  def xml2Journal(journal: NodeSeq) = {
    _Journal(
      node2Str(journal \ "ISSN"),
      node2Str(journal \ "Title")
    )
  }

  def xml2AbstractText(abstractText: NodeSeq) = {
    _AbstractText(
      node2Str(abstractText \ "@Label"),
      node2Str(abstractText \ "@NlmCategory"),
      abstractText.text
    )
  }

  def xml2Abstract(abs: NodeSeq) = {
    if (abs.isEmpty) None
    else {
      val absTexts = abs \ "AbstractText"
      Option(_Abstract(absTexts.map(xml2AbstractText).toList))
    }
  }

  def xml2Article(article: NodeSeq) = {
    _Article(
      xml2Journal(article \ "Journal"),
      node2Str(article \ "ArticleTitle"),
      xml2Abstract(article \ ""),
      xml2AuthorList(article \ "AuthorList"),
      node2Str(article \ "Title")
    )
  }

}
