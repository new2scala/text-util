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

  val testJson2 =
    """
      |{
      |    "MedlineCitation" : {
      |      "Owner" : "NLM",
      |      "Status" : "Publisher",
      |      "PMID" : "27716930",
      |      "Version" : "1",
      |      "DateCreated" : {
      |        "Year" : "2016",
      |        "Month" : "10",
      |        "Day" : "07"
      |      },
      |      "DateRevised" : {
      |        "Year" : "2016",
      |        "Month" : "10",
      |        "Day" : "08"
      |      },
      |      "Article" : {
      |        "PubModel" : "Print-Electronic",
      |        "Journal" : {
      |          "ISSN" : "1537-2995",
      |          "IssnType" : "Electronic",
      |          "JournalIssue" : {
      |            "CitedMedium" : "Internet",
      |            "PubDate" : {
      |              "Year" : "2016",
      |              "Month" : "Sep",
      |              "Day" : "09"
      |            }
      |          },
      |          "Title" : "Transfusion",
      |          "ISOAbbreviation" : "Transfusion"
      |        },
      |        "ArticleTitle" : "Self-reported historic human immunodeficiency virus (HIV) testing in a Brazilian blood donor HIV case-control study.",
      |        "ELocationID" : "10.1111/trf.13792",
      |        "ValidYN" : "Y",
      |        "EIdType" : "doi",
      |        "Abstract" : {
      |          "AbstractText" : "There has been increased worldwide emphasis on the many benefits of human immunodeficiency virus (HIV) serostatus awareness for both infection prevention and improved treatment outcomes. Previous studies indicate that donors may use blood donation to be tested; the objectives of this analysis were to assess, among donors with previously undisclosed risk behavior in the 12 months before donation, the frequency of those who have previously been tested for HIV and the demographic and behavioral factors associated with such testing.",
      |          "NlmCategory" : "BACKGROUND",
      |          "Label" : "BACKGROUND",
      |          "AbstractText" : "In this secondary analysis from an HIV case-control study of blood donors in Brazil, we analyzed the response to the question, \"Other than blood donation, have you ever been tested for HIV?\" Demographic and disclosed risk behaviors associated with previous testing were determined.",
      |          "NlmCategory" : "METHODS",
      |          "Label" : "STUDY DESIGN AND METHODS",
      |          "AbstractText" : "The study included 341 HIV-positive cases and 791 HIV-negative controls (1:2 case/control ratio). Overall, 31% of blood donors (40% of cases and 26% of controls) reported having been tested for HIV outside of blood donation. History of HIV testing varied according to sex, HIV status, and reported sexual risk behavior.",
      |          "NlmCategory" : "RESULTS",
      |          "Label" : "RESULTS",
      |          "AbstractText" : "Although it is encouraging that previous testing was more frequent in donors with acknowledged sexual risk behavior in Brazil, 60% still had not been tested for HIV outside of the blood donation setting. Educating donors on the importance of not using blood centers as a means to get tested for HIV in Brazil, especially if they engage in higher risk behaviors, and seeking alternate testing venues instead could improve the safety of donated blood.",
      |          "NlmCategory" : "CONCLUSIONS",
      |          "Label" : "CONCLUSION",
      |          "CopyrightInformation" : "© 2016 AABB."
      |        },
      |        "AuthorList" : {
      |          "CompleteYN" : "Y",
      |          "Author" : [ {
      |            "ValidYN" : "Y",
      |            "LastName" : "Bruhn",
      |            "ForeName" : "Roberta",
      |            "Initials" : "R",
      |            "Identifier" : "http://orcid.org/0000-0002-5898-8168",
      |            "Source" : "ORCID",
      |            "AffiliationInfo" : {
      |              "Affiliation" : "Blood Systems Research Institute, Epidemiology, San Francisco, California."
      |            }
      |          }, {
      |            "ValidYN" : "Y",
      |            "LastName" : "Moreno",
      |            "ForeName" : "Elizabeth",
      |            "Initials" : "E",
      |            "AffiliationInfo" : {
      |              "Affiliation" : "Fundação Hemominas/Hemocentro de Minas Gerais, Belo Horizonte, Minas Gerais, Brazil."
      |            }
      |          }, {
      |            "ValidYN" : "Y",
      |            "LastName" : "Sabino",
      |            "ForeName" : "Ester C",
      |            "Initials" : "EC",
      |            "AffiliationInfo" : {
      |              "Affiliation" : "Institute of Tropical Medicine, Universidade de São Paulo, São Paulo, Brazil."
      |            },
      |            "AffiliationInfo" : {
      |              "Affiliation" : "Department of Infectious Disease, Faculdade de Medicina, Universidade de São Paulo, São Paulo, Brazil."
      |            }
      |          }, {
      |            "ValidYN" : "Y",
      |            "LastName" : "Ferreira",
      |            "ForeName" : "Naura Aparecida F",
      |            "Initials" : "NA",
      |            "AffiliationInfo" : {
      |              "Affiliation" : "Hemocentro do Rio de Janeiro, Rio de Janeiro, Brazil."
      |            }
      |          }, {
      |            "ValidYN" : "Y",
      |            "LastName" : "Carneiro-Proietti",
      |            "ForeName" : "Anna Barbara F",
      |            "Initials" : "AB",
      |            "AffiliationInfo" : {
      |              "Affiliation" : "Fundação Hemominas/Hemocentro de Minas Gerais, Belo Horizonte, Minas Gerais, Brazil."
      |            }
      |          }, {
      |            "ValidYN" : "Y",
      |            "LastName" : "Lopes",
      |            "ForeName" : "Maria Esther D",
      |            "Initials" : "ME",
      |            "AffiliationInfo" : {
      |              "Affiliation" : "Hemocentro do Rio de Janeiro, Rio de Janeiro, Brazil."
      |            }
      |          }, {
      |            "ValidYN" : "Y",
      |            "LastName" : "Sampaio",
      |            "ForeName" : "Divaldo",
      |            "Initials" : "D",
      |            "AffiliationInfo" : {
      |              "Affiliation" : "Fundação Hemope/Hemocentro de Pernambuco, Recife, Pernambuco, Brazil."
      |            }
      |          }, {
      |            "ValidYN" : "Y",
      |            "LastName" : "Loureiro",
      |            "ForeName" : "Paula",
      |            "Initials" : "P",
      |            "AffiliationInfo" : {
      |              "Affiliation" : "Fundação Hemope/Hemocentro de Pernambuco, Recife, Pernambuco, Brazil."
      |            },
      |            "AffiliationInfo" : {
      |              "Affiliation" : "University of Pernambuco, FCM, Fundação Hemope, Recife, Pernambuco, Brazil."
      |            }
      |          }, {
      |            "ValidYN" : "Y",
      |            "LastName" : "Custer",
      |            "ForeName" : "Brian",
      |            "Initials" : "B",
      |            "AffiliationInfo" : {
      |              "Affiliation" : "Blood Systems Research Institute, Epidemiology, San Francisco, California."
      |            }
      |          }, {
      |            "ValidYN" : "Y",
      |            "LastName" : "Goncalez",
      |            "ForeName" : "Thelma T",
      |            "Initials" : "TT",
      |            "AffiliationInfo" : {
      |              "Affiliation" : "Blood Systems Research Institute, Epidemiology, San Francisco, California. tgoncalez@bloodsystems.org."
      |            }
      |          }, {
      |            "ValidYN" : "Y",
      |            "CollectiveName" : "National Heart, Lung and Blood Institute Retrovirus Epidemiology Donor Study-II (REDS-II) International Component"
      |          } ]
      |        },
      |        "Language" : "eng",
      |        "PublicationTypeList" : {
      |          "PublicationType" : "Journal Article",
      |          "UI" : ""
      |        },
      |        "ArticleDate" : {
      |          "DateType" : "Electronic",
      |          "Year" : "2016",
      |          "Month" : "09",
      |          "Day" : "09"
      |        }
      |      },
      |      "MedlineJournalInfo" : {
      |        "Country" : "United States",
      |        "MedlineTA" : "Transfusion",
      |        "NlmUniqueID" : "0417360",
      |        "ISSNLinking" : "0041-1132"
      |      }
      |    },
      |    "PubmedData" : {
      |      "History" : {
      |        "PubMedPubDate" : [ {
      |          "PubStatus" : "received",
      |          "Year" : "2016",
      |          "Month" : "02",
      |          "Day" : "24"
      |        }, {
      |          "PubStatus" : "revised",
      |          "Year" : "2016",
      |          "Month" : "06",
      |          "Day" : "06"
      |        }, {
      |          "PubStatus" : "accepted",
      |          "Year" : "2016",
      |          "Month" : "06",
      |          "Day" : "28"
      |        }, {
      |          "PubStatus" : "entrez",
      |          "Year" : "2016",
      |          "Month" : "10",
      |          "Day" : "8",
      |          "Hour" : "6",
      |          "Minute" : "0"
      |        }, {
      |          "PubStatus" : "pubmed",
      |          "Year" : "2016",
      |          "Month" : "10",
      |          "Day" : "8",
      |          "Hour" : "6",
      |          "Minute" : "0"
      |        }, {
      |          "PubStatus" : "medline",
      |          "Year" : "2016",
      |          "Month" : "10",
      |          "Day" : "8",
      |          "Hour" : "6",
      |          "Minute" : "0"
      |        } ]
      |      },
      |      "PublicationStatus" : "aheadofprint",
      |      "ArticleIdList" : {
      |        "ArticleId" : [ {
      |          "ArticleId" : "27716930",
      |          "IdType" : "pubmed"
      |        }, {
      |          "ArticleId" : "10.1111/trf.13792",
      |          "IdType" : "doi"
      |        } ]
      |      }
      |    }
      |  }
      |
    """.stripMargin
  import PubmedArticle._
  "json test" should "pass" in {
    var article = fromJson(testJson)
    article.PubmedData.History.PubMedPubDate.size shouldBe 3
    article = fromJson(testJson2)
    article.PubmedData.History.PubMedPubDate.size shouldBe 3
  }
}
