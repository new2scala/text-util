package org.dele.text.lapa

import org.dele.text.lapa.patterns.DomainLists._
import org.dele.text.lapa.patterns.DomainStructure._
import org.dele.text.lapa.patterns.PtnUtil._
import org.dele.text.lapa.patterns.{AppliedPattern, TLangPattern}
import org.dele.text.lapa.patterns.TLangPattern.PtnNode
import org.dele.text.maen.matchers.MatcherManager._
import org.dele.text.maen.{TAtom, TInput}
import org.dele.text.maen.matchers.{StoppedByMatcherManager, SubMatchCheckerLib}
import org.dele.text.maen.test.TestAtom._
import org.dele.text.maen.test.TestInput
import org.dele.text.lapa.patterns.{AppliedPattern, TLangPattern}

/**
  * Created by jiaji on 2016-02-12.
  */
object TestHelper {
  import org.dele.text.lapa.patterns.TLangPattern._
  val testPatternDomainId = "-non-intrusive-online-attack"
  def d(domainName:String, n:String) = "%s.%s".format(domainName, n)
  val testPatternId = "ATTACKER-launch-[METHOD]-attack-against-TARGET"

  val ptnDomainJson =
    """
      |{
      | "id": "%s",
      | "patternGroups": [
      | {
      |  "id": "%s",
      |  "languages": [ "eng", "zhs" ],
      |  "patterns": [
      |   {
      |    "nodes": [
      |     { "id": "attacker", "content": "ALL-ATTACKERS" },
      |     { "id": "launch", "content": "launch-words" },
      |     { "id": "[method]-attack", "content": "[METHOD]-attack" },
      |     { "id": "against", "content": "against-words" },
      |     { "id": "target", "content": "ALL-TARGETS" }
      |    ]
      |   }
      |  ]
      | },
      | {
      |  "id": "[METHOD]-attack",
      |  "languages": [ "eng", "zhs" ],
      |  "defLineTypes": [ "lngChecker", "clause1" ],
      |  "patterns": [
      |   {
      |    "id": "METHOD-attack",
      |    "nodes": [
      |     { "id": "method", "content": "METHOD" },
      |     { "id": "attack", "content": "attack-words" }
      |    ]
      |   },
      |   {
      |    "id": "attack",
      |    "nodes": [
      |     { "id": "attack", "content": "attack-words" }
      |    ]
      |   }
      |  ]
      | },
      | {
      |  "id": "ALL-ATTACKERS",
      |  "languages": [ "eng", "zhs" ],
      |  "defLineTypes": [ "lngChecker", "clause1" ],
      |  "patterns": [
      |   {
      |    "id": "OrgAttacker",
      |    "defLineTypes": [ "lngChecker" ],
      |    "nodes": [
      |     { "id": "org-attacker", "content": "org-list" }
      |    ]
      |   },
      |   {
      |    "id": "CountryAttacker",
      |    "languages": [ "zhs" ],
      |    "nodes": [
      |     { "id": "country-attacker", "content": "country-list" }
      |    ]
      |   }
      |  ]
      | },
      | {
      |  "id": "METHOD",
      |  "patterns": [
      |   {
      |    "id": "AttackVectorMethod",
      |    "languages": [ "eng", "zhs" ],
      |    "nodes": [
      |     { "id": "attack-vector-method", "content": "attack-vector-list" }
      |    ]
      |   },
      |   {
      |    "id": "CountryAttacker",
      |    "languages": [ "eng", "zhs" ],
      |    "nodes": [
      |     { "id": "country-attacker", "content": "country-list" }
      |    ]
      |   }
      |  ]
      | },
      | {
      |  "id": "ALL-TARGETS",
      |  "patterns": [
      |   {
      |    "id": "OrgCmpTarget",
      |    "languages": [ "eng", "zhs" ],
      |    "nodes": [
      |     { "id": "org-cmp-target", "content": "org-cmp-list" }
      |    ]
      |   },
      |   {
      |    "id": "CountryTarget",
      |    "languages": [ "eng", "zhs" ],
      |    "nodes": [
      |     { "id": "country-target", "content": "country-list" }
      |    ]
      |   }
      |  ]
      | }]
      |}
    """.stripMargin.format(testPatternDomainId, testPatternId)

  val engAppliedPatternJson =
    """
      |{
      | "lang": "eng",
      | "defaultMatchCheckerId": "clause2",
      | "appliedPatternDomains": [ ]
      |}
    """.stripMargin

  val engAppliedPatterns = AppliedPattern.fromJson(engAppliedPatternJson)

  val zhsAppliedPatternJson =
    """
      |{
      | "lang": "zhs",
      | "defaultMatchCheckerId": "clause2",
      | "appliedPatternDomains": [
      |  {
      |   "domain": "%s",
      |   "appliedPatterns": [
      |    {
      |     "ptnId": "%s",
      |     "nodeOrders": [
      |      [ "attacker", "against", "target", "launch", "[method]-attack" ],
      |      [ "attacker", "launch", "against", "target", "[method]-attack" ]
      |     ],
      |     "lineTypeId": [ ]
      |    }
      |   ]
      |  }
      | ]
      |}
    """.stripMargin.format(testPatternDomainId, testPatternId)

  val zhsAppliedPatterns = AppliedPattern.fromJson(zhsAppliedPatternJson)

  val patternDomain = TLangPattern.parsePatternDomain(ptnDomainJson)
  /*
  val patternGroup1 = TLangPattern.parseLangPatternGroup(ptnGroup1Json)
  val patternGroup2 = TLangPattern.parseLangPatternGroup(ptnGroup2Json)
  val patternGroup3 = TLangPattern.parseLangPatternGroup(ptnGroup3Json)
  val patternGroup4 = TLangPattern.parseLangPatternGroup(ptnGroup4Json)
  val testPattern1 = patternGroup1.patterns(0)
  val testPattern2 = patternGroup2.patterns(1)
  val testPattern3 = patternGroup3.patterns(0)
  val testPattern4 = patternGroup4.patterns(0)
  */


  import TestInput._

  val FBI = entityAtom("FBI", "Organization", "OrgEntity")
  val Anonymous = entityAtom("Anonymous", "Organization", "OrgEntity")
  val Anomymous_zhs = entityAtom("匿名者", "Organization", "OrgEntity")
  val DDoS = entityAtom("DDoS", "AttackVector")
  val launch = textAtom("launch")
  val attack = textAtom("attack")
  val Microsoft = entityAtom("Microsoft", "Company", "OrgEntity")
  val engInput1:TInput = fromAtomArrayEng(IndexedSeq(
    textAtom("Now"),
    Anonymous,
    launch,
    DDoS,
    attack,
    textAtom("against"),
    FBI,
    textAtom("and"),
    Microsoft
  ))

  val engInput2:TInput = fromAtomArrayEng(IndexedSeq(
    FBI,
    textAtom("hit"),
    textAtom("by"),
    textAtom("a"),
    textAtom("new"),
    DDoS,
    attack
  ))

  val engInput3:TInput = fromAtomArrayEng(IndexedSeq(
    FBI,
    textAtom("targeted"),
    textAtom("in"),
    textAtom("a"),
    textAtom("new"),
    DDoS,
    attack
  ))

  def fromAtomArrayZhs(atoms:IndexedSeq[TAtom]) = fromAtomArray("zhs", atoms)

  val against_zhs = textAtom("针对")
  val launch_zhs = textAtom("发起")
  val attack_zhs = textAtom("攻击")
  val zhsInput1:TInput = fromAtomArrayZhs(IndexedSeq(
    textAtom("最近"),
    Anomymous_zhs,
    launch_zhs,
    against_zhs,
    FBI,
    textAtom("和"),
    Microsoft,
    DDoS,
    attack_zhs
  ))


  val zhsInput2:TInput = fromAtomArrayZhs(IndexedSeq(
    textAtom("最近"),
    Anomymous_zhs,
    against_zhs,
    FBI,
    textAtom("和"),
    Microsoft,
    launch_zhs,
    DDoS,
    attack_zhs
  ))

  val zhsInput3:TInput = fromAtomArrayZhs(IndexedSeq(
    textAtom("最近"),
    FBI,
    textAtom("遭受"),
    textAtom("新一轮"),
    DDoS,
    attack_zhs
  ))

  val zhsInput4:TInput = fromAtomArrayZhs(IndexedSeq(
    textAtom("最近"),
    FBI,
    textAtom("遭受"),
    textAtom("新一轮"),
    textAtom("网络攻击")
  ))


  val domainTree =
    """
      |[
      | {
      |  "id": "_root_",
      |  "childIds": [ "cyber-attack", "-online-attack", "-cyber-operation" ]
      | },
      | {
      |  "id": "cyber-attack",
      |  "childIds": [ "-data-breach", "military-maneuver" ]
      | },
      | {
      |  "id": "military-maneuver",
      |  "childIds": [ "-military-exercise", "-military-deployment" ]
      | },
      | {
      |  "id": "-online-attack",
      |  "childIds": [ "-intrusive-online-attack", "-non-intrusive-online-attack" ]
      | }

      |]
    """.stripMargin

  val domainStructure = load(domainTree, List())

  val engTestLists =
    """
      |{
      | "lang": "eng",
      | "domainLists": [
      |  {
      |   "domain": "cyber-attack",
      |   "lists": [
      |    { "id": "attack-words", "lines": [ [ "F(attack)" ] ] },
      |    { "id": "against-words", "lines": [ [ "F(against)" ] ] },
      |    { "id": "org-list", "lines": [ [ "E(Organization)" ] ] },
      |    { "id": "org-cmp-list", "lines": [ [ "E(Organization / Company)" ] ] },
      |    { "id": "country-list", "lines": [ [ "E(Country)" ] ] },
      |    { "id": "attack-vector-list", "lines": [ [ "E(AttackVector)" ] ] }
      |   ]
      |  },
      |  {
      |   "domain": "-online-attack",
      |   "lists": [
      |    { "id": "launch-words", "lines": [ [ "F(launch)" ] ] }
      |   ]
      |  },
      |  {
      |   "domain": "_root_",
      |   "lists": [
      |    { "id": "entity-list-connector-words", "lines": [ [ "F(and / or / ,)" ] ] }
      |   ]
      |  }
      | ]
      |}
    """.stripMargin
  val engDomainLists = loadDomainLists(engTestLists)

  val EmptySubMatchCheckerLib = new SubMatchCheckerLib(List(), List())

  implicit val exGlobalIds = Map[String,List[String]]()
  val engDomainMgr = new LangDomainManager(domainStructure, List(patternDomain), engDomainLists, EmptySubMatchCheckerLib)

  val zhsTestLists =
    """
      |{
      | "lang": "zhs",
      | "domainLists": [
      |  {
      |   "domain": "cyber-attack",
      |   "lists": [
      |    { "id": "attack-words", "lines": [ [ "F(攻击)" ] ] },
      |    { "id": "against-words", "lines": [ [ "F(针对)" ] ] },
      |    { "id": "org-list", "lines": [ [ "E(Organization)" ] ] },
      |    { "id": "org-cmp-list", "lines": [ [ "E(Organization / Company)" ] ] },
      |    { "id": "country-list", "lines": [ [ "E(Country)" ] ] },
      |    { "id": "attack-vector-list", "lines": [ [ "E(AttackVector)" ] ] }
      |   ]
      |  },
      |  {
      |   "domain": "-online-attack",
      |   "lists": [
      |    { "id": "launch-words", "lines": [ [ "F(发起)" ] ] }
      |   ]
      |  }
      | ]
      |}
    """.stripMargin
  val zhsDomainLists = loadDomainLists(zhsTestLists)

  val zhsDomainMgr = new LangDomainManager(domainStructure, List(patternDomain), zhsDomainLists, EmptySubMatchCheckerLib)
}
