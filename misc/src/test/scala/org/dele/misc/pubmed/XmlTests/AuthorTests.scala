package org.dele.misc.pubmed.XmlTests

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by dele on 2017-02-28.
  */
class AuthorTests extends FlatSpec with Matchers with TableDrivenPropertyChecks {

  val author1 = """
      |<Author ValidYN="Y">
      |            <LastName>Stephens</LastName>
      |            <ForeName>Christopher R</ForeName>
      |            <Initials>CR</Initials>
      |            <AffiliationInfo>
      |              <Affiliation>Instituto de Ciencias Nucleares, Universidad Nacional Autónoma de México, Ciudad de Mexico, Mexico.</Affiliation>
      |            </AffiliationInfo>
      |            <AffiliationInfo>
      |              <Affiliation>C3 - Centro de Ciencias de la Complejidad, Universidad Nacional Autónoma de México, Ciudad de Mexico, Mexico.</Affiliation>
      |            </AffiliationInfo>
      |</Author>
    """.stripMargin

  val author2 =
    """
      |<Author ValidYN="Y">
      |            <LastName>González-Salazar</LastName>
      |            <ForeName>Constantino</ForeName>
      |            <Initials>C</Initials>
      |            <Identifier Source="ORCID">http://orcid.org/0000-0001-7347-714X</Identifier>
      |            <AffiliationInfo>
      |              <Affiliation>C3 - Centro de Ciencias de la Complejidad, Universidad Nacional Autónoma de México, Ciudad de Mexico, Mexico.</Affiliation>
      |            </AffiliationInfo>
      |</Author>
    """.stripMargin

  val author3 =
    """
      |<Author ValidYN="Y">
      |            <LastName>McMartin</LastName>
      |            <ForeName>K E</ForeName>
      |            <Initials>KE</Initials>
      |</Author>
    """.stripMargin

  val author31 =
    """
      |<Author ValidYN="Y" UnknownAttr="ua">
      |            <LastName>McMartin</LastName>
      |            <ForeName>K E</ForeName>
      |            <Initials>KE</Initials>
      |            <Unknown>test</Unknown>
      |</Author>
    """.stripMargin

  val author4 =
    """
      |<Author ValidYN="Y">
      |            <LastName>Wright</LastName>
      |            <ForeName>Eugene E</ForeName>
      |            <Initials>EE</Initials>
      |            <Suffix>Jr</Suffix>
      |            <AffiliationInfo>
      |              <Affiliation>Department of Medicine, Duke University Medical Center at the Southern Regional Area Health Education Center (AHEC), Fayetteville, North Carolina.</Affiliation>
      |            </AffiliationInfo>
      |            <AffiliationInfo>
      |              <Affiliation>Department of Community and Family Medicine, Duke University Medical Center at the Southern Regional Area Health Education Center (AHEC), Fayetteville, North Carolina.</Affiliation>
      |            </AffiliationInfo>
      |</Author>
    """.stripMargin

  val testData = Table(
    ("xml", "lastName", "foreName", "initials", "suffix", "identifierSource", "identifier", "firstAffiliation" ),
    (author1, "Stephens", "Christopher R", "CR", None, None, None, Option("Instituto de Ciencias Nucleares, Universidad Nacional Autónoma de México, Ciudad de Mexico, Mexico.")),
    (author2, "González-Salazar", "Constantino", "C", None, Option("ORCID"), "http://orcid.org/0000-0001-7347-714X", Option("C3 - Centro de Ciencias de la Complejidad, Universidad Nacional Autónoma de México, Ciudad de Mexico, Mexico.")),
    (author3, "McMartin", "K E", "KE", None, None, None, None),
    (author31, "McMartin", "K E", "KE", None, None, None, None),
    (author4, "Wright", "Eugene E", "EE", Option("Jr"), None, None, Option("Department of Medicine, Duke University Medical Center at the Southern Regional Area Health Education Center (AHEC), Fayetteville, North Carolina."))
  )

  import org.dele.misc.pubmed.PubmedXmlHelpers._
  "authors tests" should "pass" in {
    forAll(testData) { (xml,lastName,foreName,initials, suffix, identifierSource,identifier,firstAffiliation ) =>
      val xmlNode = scala.xml.XML.loadString(xml)
      val author = xml2Author(xmlNode)

      author.LastName.get shouldBe lastName
      author.ForeName.get shouldBe foreName
      author.Initials.get shouldBe initials
      author.Suffix shouldBe suffix
      if (author.Identifier.isEmpty) {
        identifierSource shouldBe None
        identifier shouldBe None
      }
      else {
        val id = author.Identifier.get
        id.Source shouldBe identifierSource
        id.Content shouldBe identifier
      }
      if (firstAffiliation.nonEmpty) author.AffiliationInfo.head.Affiliation shouldBe firstAffiliation.get
      else author.AffiliationInfo.size shouldBe 0
    }
  }
}
