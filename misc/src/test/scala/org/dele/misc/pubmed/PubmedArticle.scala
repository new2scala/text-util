package org.dele.misc.pubmed

import org.json4s.{DefaultFormats, FieldSerializer}

/**
  * Created by dele on 2017-02-23.
  */

class _AffiliationInfo(Affiliation: String)
class _Author(
               val ValidYN: String,
               val LastName: Option[String],
               val ForeName: Option[String],
               val Suffix: Option[String],
               val Initials: Option[String],
               val AffiliationInfo: List[_AffiliationInfo],
               val CollectiveName: Option[String]) {
  def isIndividual = CollectiveName.isEmpty
  def isCollective = CollectiveName.nonEmpty
}
class _AuthorList(val CompleteYN: String, val Author: List[_Author])

class _Grant(val GrantId:String, val Agency: String, val Country: String)
class _GrantList(val Grant: List[_Grant])

class _MeshHeading(val DescriptorName: String, val QualifierName: String)

class _Journal(val Title: String)

class _Article(val Journal: _Journal, val ArticleTitle: String, val AuthorList: _AuthorList, val Language: String)

class _MedlineCitation(val PMID: String, val Article: _Article)

class _PubMedPubDate(val PubStatus: String, val Year:Option[String], val Month: Option[String], val Day:Option[String], val Hour: Option[String], val Minute:Option[String])
class _History(val PubMedPubDate: List[_PubMedPubDate])
class _PubmedData(val History: _History)

class _PubmedArticle(val MedlineCitation: _MedlineCitation, val PubmedData: _PubmedData ) {

}

object PubmedArticle {
  import org.json4s.jackson.Serialization._
  import org.json4s.jackson.JsonMethods._

  val userSerializer = FieldSerializer[_PubmedArticle](
    FieldSerializer.ignore("MedlineCitation")
  )
  implicit val format = DefaultFormats
  def fromJson(j:String):_PubmedArticle = {

    parse(j).extract[_PubmedArticle]
  }
}