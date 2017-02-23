package org.dele.misc.pubmed

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by dele on 2017-02-23.
  */
class PubmedArticleJsonTest extends FlatSpec with Matchers {
  val testJson =
    """
      |{
      |    "MedlineCitation" : {
      |      "Owner" : "NLM",
      |      "Status" : "MEDLINE",
      |      "PMID" : "1",
      |      "Version" : "1",
      |      "DateCreated" : {
      |        "Year" : "1976",
      |        "Month" : "01",
      |        "Day" : "16"
      |      },
      |      "DateCompleted" : {
      |        "Year" : "1976",
      |        "Month" : "01",
      |        "Day" : "16"
      |      },
      |      "DateRevised" : {
      |        "Year" : "2016",
      |        "Month" : "12",
      |        "Day" : "03"
      |      },
      |      "Article" : {
      |        "PubModel" : "Print",
      |        "Journal" : {
      |          "ISSN" : "0006-2944",
      |          "IssnType" : "Print",
      |          "JournalIssue" : {
      |            "CitedMedium" : "Print",
      |            "Volume" : "13",
      |            "Issue" : "2",
      |            "PubDate" : {
      |              "Year" : "1975",
      |              "Month" : "Jun"
      |            }
      |          },
      |          "Title" : "Biochemical medicine",
      |          "ISOAbbreviation" : "Biochem Med"
      |        },
      |        "ArticleTitle" : "Formate assay in body fluids: application in methanol poisoning.",
      |        "Pagination" : {
      |          "MedlinePgn" : "117-26"
      |        },
      |        "AuthorList" : {
      |          "CompleteYN" : "Y",
      |          "Author" : [ {
      |            "ValidYN" : "Y",
      |            "LastName" : "Makar",
      |            "ForeName" : "A B",
      |            "Initials" : "AB"
      |          }, {
      |            "ValidYN" : "Y",
      |            "LastName" : "McMartin",
      |            "ForeName" : "K E",
      |            "Initials" : "KE"
      |          }, {
      |            "ValidYN" : "Y",
      |            "LastName" : "Palese",
      |            "ForeName" : "M",
      |            "Initials" : "M"
      |          }, {
      |            "ValidYN" : "Y",
      |            "LastName" : "Tephly",
      |            "ForeName" : "T R",
      |            "Initials" : "TR"
      |          } ]
      |        },
      |        "Language" : "eng",
      |        "GrantList" : {
      |          "CompleteYN" : "Y",
      |          "Grant" : {
      |            "GrantID" : "MC_UU_12013/5",
      |            "Agency" : "MRC",
      |            "Country" : "United Kingdom"
      |          }
      |        },
      |        "PublicationTypeList" : {
      |          "PublicationType" : [ {
      |            "PublicationType" : "Journal Article",
      |            "UI" : "D016428"
      |          }, {
      |            "PublicationType" : "Research Support, U.S. Gov't, P.H.S.",
      |            "UI" : "D013487"
      |          } ]
      |        }
      |      },
      |      "MedlineJournalInfo" : {
      |        "Country" : "United States",
      |        "MedlineTA" : "Biochem Med",
      |        "NlmUniqueID" : "0151424",
      |        "ISSNLinking" : "0006-2944"
      |      },
      |      "ChemicalList" : {
      |        "Chemical" : [ {
      |          "RegistryNumber" : "0",
      |          "NameOfSubstance" : "Formates",
      |          "UI" : "D005561"
      |        }, {
      |          "RegistryNumber" : "142M471B3J",
      |          "NameOfSubstance" : "Carbon Dioxide",
      |          "UI" : "D002245"
      |        }, {
      |          "RegistryNumber" : "EC 1.2.-",
      |          "NameOfSubstance" : "Aldehyde Oxidoreductases",
      |          "UI" : "D000445"
      |        }, {
      |          "RegistryNumber" : "Y4S76JWI15",
      |          "NameOfSubstance" : "Methanol",
      |          "UI" : "D000432"
      |        } ]
      |      },
      |      "CitationSubset" : "IM",
      |      "MeshHeadingList" : {
      |        "MeshHeading" : [ {
      |          "DescriptorName" : "Aldehyde Oxidoreductases",
      |          "MajorTopicYN" : "N",
      |          "UI" : "D000445",
      |          "QualifierName" : "metabolism",
      |          "MajorTopicYN" : "N",
      |          "UI" : "Q000378"
      |        }, {
      |          "DescriptorName" : "Animals",
      |          "MajorTopicYN" : "N",
      |          "UI" : "D000818"
      |        }, {
      |          "DescriptorName" : "Body Fluids",
      |          "MajorTopicYN" : "N",
      |          "UI" : "D001826",
      |          "QualifierName" : "analysis",
      |          "MajorTopicYN" : "Y",
      |          "UI" : "Q000032"
      |        }, {
      |          "DescriptorName" : "Carbon Dioxide",
      |          "MajorTopicYN" : "N",
      |          "UI" : "D002245",
      |          "QualifierName" : "blood",
      |          "MajorTopicYN" : "N",
      |          "UI" : "Q000097"
      |        }, {
      |          "DescriptorName" : "Formates",
      |          "MajorTopicYN" : "N",
      |          "UI" : "D005561",
      |          "QualifierName" : "blood",
      |          "MajorTopicYN" : "N",
      |          "UI" : "Q000097",
      |          "QualifierName" : "poisoning",
      |          "MajorTopicYN" : "Y",
      |          "UI" : "Q000506"
      |        }, {
      |          "DescriptorName" : "Haplorhini",
      |          "MajorTopicYN" : "N",
      |          "UI" : "D000882"
      |        }, {
      |          "DescriptorName" : "Humans",
      |          "MajorTopicYN" : "N",
      |          "UI" : "D006801"
      |        }, {
      |          "DescriptorName" : "Hydrogen-Ion Concentration",
      |          "MajorTopicYN" : "N",
      |          "UI" : "D006863"
      |        }, {
      |          "DescriptorName" : "Kinetics",
      |          "MajorTopicYN" : "N",
      |          "UI" : "D007700"
      |        }, {
      |          "DescriptorName" : "Methanol",
      |          "MajorTopicYN" : "N",
      |          "UI" : "D000432",
      |          "QualifierName" : "blood",
      |          "MajorTopicYN" : "N",
      |          "UI" : "Q000097"
      |        }, {
      |          "DescriptorName" : "Methods",
      |          "MajorTopicYN" : "N",
      |          "UI" : "D008722"
      |        }, {
      |          "DescriptorName" : "Pseudomonas",
      |          "MajorTopicYN" : "N",
      |          "UI" : "D011549",
      |          "QualifierName" : "enzymology",
      |          "MajorTopicYN" : "N",
      |          "UI" : "Q000201"
      |        } ]
      |      }
      |    },
      |    "PubmedData" : {
      |      "History" : {
      |        "PubMedPubDate" : [ {
      |          "PubStatus" : "pubmed",
      |          "Year" : "1975",
      |          "Month" : "6",
      |          "Day" : "1"
      |        }, {
      |          "PubStatus" : "medline",
      |          "Year" : "1975",
      |          "Month" : "6",
      |          "Day" : "1",
      |          "Hour" : "0",
      |          "Minute" : "1"
      |        }, {
      |          "PubStatus" : "entrez",
      |          "Year" : "1975",
      |          "Month" : "6",
      |          "Day" : "1",
      |          "Hour" : "0",
      |          "Minute" : "0"
      |        } ]
      |      },
      |      "PublicationStatus" : "ppublish",
      |      "ArticleIdList" : {
      |        "ArticleId" : "1",
      |        "IdType" : "pubmed"
      |      }
      |    }
      |}
      |
    """.stripMargin
  import PubmedArticle._
  "json test" should "pass" in {
    val article = fromJson(testJson)
    article.PubmedData.History.PubMedPubDate.size shouldBe 3
  }
}
