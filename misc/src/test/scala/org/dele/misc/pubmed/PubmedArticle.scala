package org.dele.misc.pubmed

import org.json4s.{DefaultFormats, FieldSerializer}

import scala.xml.NodeSeq

/**
  * Created by dele on 2017-02-23.
  */

import PubmedArticle._

case class _Identifier(
                      Source:Option[String],
                      Content:String
                      )
case class _AffiliationInfo(Affiliation: String)
case class _Author(
               ValidYN: Option[String] = None,
               LastName: Option[String] = None,
               ForeName: Option[String] = None,
               Suffix: Option[String] = None,
               Initials: Option[String] = None,
               Identifier: Option[_Identifier],
               AffiliationInfo: List[_AffiliationInfo] = EmptyAffiliationList,
               CollectiveName: Option[String] = None) {
  def isIndividual = CollectiveName.isEmpty
  def isCollective = CollectiveName.nonEmpty
}

case class _AuthorList(CompleteYN: Option[String], Authors: List[_Author])

class _Grant(val GrantId:String, val Agency: String, val Country: String)
class _GrantList(val Grant: List[_Grant])

class _MeshHeading(val DescriptorName: String, val QualifierName: String)

case class _Journal(
                     ISSN: Option[String],
                     Title: Option[String]
                   )

case class _AbstractText(
                        Label: Option[String],
                        NlmCategory: Option[String],
                        Text: String
                        )
case class _Abstract(
                      AbstractText: List[_AbstractText]
                    )
case class _Article(
                     Journal: _Journal,
                     ArticleTitle: Option[String],
                     Abstract: Option[_Abstract],
                     AuthorList: _AuthorList,
                     Language: Option[String]
                   )

class _MedlineCitation(val PMID: String, val Article: _Article)

case class _PubMedPubDate(
                           PubStatus: Option[String],
                           Year:Option[String],
                           Month: Option[String],
                           Day:Option[String],
                           Hour: Option[String],
                           Minute:Option[String]
                         )
case class _History(PubMedPubDate: List[_PubMedPubDate])
case class _PubmedData(History: _History)

case class _PubmedArticle(MedlineCitation: _MedlineCitation, PubmedData: _PubmedData ) {
}

object PubmedArticle {

  import org.json4s.jackson.JsonMethods._

  val EmptyAffiliationList = List[_AffiliationInfo]()
  val EmptyPubDateList = List[_PubMedPubDate]()

  val userSerializer = FieldSerializer[_PubmedArticle](
    FieldSerializer.ignore("MedlineCitation")
  )
  implicit val format = DefaultFormats
  def fromJson(j:String):_PubmedArticle = {
    parse(j).extract[_PubmedArticle]
  }
}