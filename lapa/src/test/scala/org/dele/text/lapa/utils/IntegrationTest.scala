package org.dele.text.lapa.utils

import org.dele.text.lapa.TestHelper
import org.dele.text.lapa.patterns.AppliedPattern.AppliedPattern4Lang
import org.dele.text.lapa.patterns.DomainLists.LangDomainLists
import org.dele.text.lapa.patterns.DomainStructure.LangDomainManager
import org.dele.text.lapa.patterns._
import org.dele.text.lapa.patterns.TLangPattern.LangPatternDomain
import org.dele.text.maen.AtomPropMatcherLib.EmptyRegexDict
import org.dele.text.maen.AtomPropMatcherLib._
import org.dele.text.maen.TInput
import org.dele.text.maen.extracts.Extract
import org.dele.text.maen.extracts.Extract._
import org.dele.text.maen.matchers.MatcherManager
import org.dele.text.lapa.patterns._
import org.dele.text.lapa.patterns.AppliedPattern.AppliedPattern4Lang

import scala.io.Source

/**
  * Created by jiaji on 2016-02-22.
  */
object IntegrationTest extends App {
  def file2String(path:String):String = Source.fromFile(path, "UTF-8").mkString

  def loadPatternDomain(path:String):LangPatternDomain = TLangPattern.parsePatternDomain(file2String(path))

  def loadAppliedPatterns(path:String):AppliedPattern4Lang = AppliedPattern.fromJson(file2String(path))

  def loadDomainLists(path:String):LangDomainLists = DomainLists.loadDomainLists(file2String(path))
  val rootDomainName = "cyber-attack"
  def loadExtracts(path:String):ExtractDomain = Extract.fromJson(file2String(path)).getExtractDefSet(rootDomainName)

  import org.dele.text.lapa.TestHelper._
  import org.dele.text.lapa.patterns.LangPatternGroupTemplate._
  def load4Lang(lang:String):MatcherManager = {
    val zhsmm = MatcherManager.create
    val zhsAppPatterns = loadAppliedPatterns(langAppFile(lang))
    val zhsLists = loadDomainLists(langLists(lang))
    val zhsDomainMgr = new LangDomainManager(domainStructure, List(onlineAttackPatterns, cyberAttackPatterns), zhsLists, EmptySubMatchCheckerLib)
    implicit val smcLib = EmptySubMatchCheckerLib

    zhsAppPatterns.applyAllDomains(zhsDomainMgr, TMatcherGen.NoMatcherTemplateLibGen, EmptyPatternTemplateLib, EmptyRegexDict).foreach(zhsmm.add)
    zhsDomainMgr.listMatchers(TMatcherGen.NoMatcherTemplateLibGen, EmptyRegexDict).foreach(zhsmm.add)
    zhsmm
  }

  val baseDir = "/home/jiaji/devsettings/language-patterns/"
  val domainStructureFile = baseDir + "domain-structure.json"
  val domainStructure = DomainStructure.load(file2String(domainStructureFile), List())
  val appliedDir = baseDir + "applied/"
  val lcZhs = "zhs"
  val lcEng = "eng"
  def langAppDir(lang:String) = appliedDir + lang + "/"
  def langAppFile(lang:String) = langAppDir(lang) + "patterns-applied.json"
  def langLists(lang:String) = langAppDir(lang) + "domain-lists.json"
  val zhsAppDir = langAppDir(lcZhs)
  val engAppDir = langAppDir(lcEng)
  def patternDomainDir(domain:String) = baseDir + domain + "/"
  val domain = "cyber-attack"
  val cyberPatternDomainDir = patternDomainDir(domain)


  val onlineAttackPatterns = loadPatternDomain(cyberPatternDomainDir + "non-intrusive-online-attack.json")
  val cyberAttackPatterns = loadPatternDomain(cyberPatternDomainDir + "cyber-attack.json")
  val extracts = loadExtracts(cyberPatternDomainDir + "extracts.json")

  //val patternId1 = onlineAttackPatterns.patternGroups.flatMap(_.patterns).take(1)(0).getId
  //val patternId2 = onlineAttackPatterns.patternGroups.flatMap(_.patterns).take(1)(0).getId
  val patternDomainId = onlineAttackPatterns.id
  val ptnIds:List[String] = onlineAttackPatterns.patternGroups.flatMap(_.patternIds).map(DomainStructure.genId(patternDomainId, _))
  //val ptnId2:String = DomainStructure.genId(patternDomainId, onlineAttackPatterns.patternGroups.flatMap(_.patternIds).take(2)(1))

  def runTest(mm:MatcherManager, input:TInput, ptnId:String): Unit = {
    val resultPool = mm.m(input, EmptySubMatchCheckerLib, MatcherManager.EmptyMIdFilters)
    val events = resultPool.query(ptnId)
    //println(events.mkString("\n"))
    events.foreach(
      evt => {
        println(evt)
        println("----------- extracts:")
        val extr = extracts.run(evt, EmptyRelatedEntityCheckerIds)
        println(extr.map(_._2.toString).mkString("\n"))
        println("----------- end of extracts")
      }
    )

    println
  }

  import org.dele.text.lapa.TestHelper._

  val zhsmm = load4Lang(lcZhs)
  runTest(zhsmm, zhsInput1, ptnIds(0))
  runTest(zhsmm, zhsInput2, ptnIds(0))
  runTest(zhsmm, zhsInput3, ptnIds(1))
  runTest(zhsmm, zhsInput4, ptnIds(1))

  val engmm = load4Lang(lcEng)
  runTest(engmm, engInput1, ptnIds(0))
  runTest(engmm, engInput2, ptnIds(1))
  runTest(engmm, engInput3, ptnIds(1))

}
