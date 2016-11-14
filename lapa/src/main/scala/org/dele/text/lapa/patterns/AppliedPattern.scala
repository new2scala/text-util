package org.dele.text.lapa.patterns

import org.dele.text.lapa.patterns.DomainStructure.LangDomainManager
import org.dele.text.lapa.patterns.LangPatternGroupTemplate.LangPatternGroupTemplateLib
import org.dele.text.lapa.patterns.TLangPattern._
import org.dele.text.maen.ConfValueStringParser
import org.dele.text.maen.ConfValueStringParser.Parsed
import org.dele.text.maen.matchers.{SubMatchCheckerLib, TMatcher}
import org.dele.text.maen.matchers.TMatcher._
import org.dele.text.lapa.patterns.LangPatternGroupTemplate.LangPatternGroupTemplateLib
import org.json4s.NoTypeHints
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization

/**
  * Created by jiaji on 2016-02-15.
  */
class AppliedPattern (val ptnId:String, val nodeOrders:List[List[PNId]], val lineTypes:List[String]) {
  import AppliedPattern._
  private var _owner:AppliedPattern4Lang = null

  private[AppliedPattern] def init(owner:AppliedPattern4Lang):Unit = _owner = owner
  def lang:String = _owner.lang
}

object AppliedPattern {

  private def NodeOrder(ptnId:String, idx:Int) = "%s.NodeOrder#%02d".format(ptnId, idx)
  //private def nodeMatcherName(ptn:TLangPattern, nodeContent:String) = "%s.%s".format(ptn.getDomain, nodeContent)
  import DomainStructure.genId

  private def nodeContentId(domainMgr:LangDomainManager, domain:String, id:String):String = domainMgr.getFullId(domain, id)
  private def nodeContentMatcher(domainMgr:LangDomainManager, regexDict:Map[String,String], matcherGen:TMatcherGen, domain:String, node:PtnNode)
                                (implicit subMatchCheckerLib: SubMatchCheckerLib) = {
    //val parsed = ConfValueStringParser.parse(node.content)
    val matchers = node.contentEntries.map{ p =>
      val entry = p._1
      val parsed = p._2
      if (parsed.paras.isEmpty) queryPoolMatcher(nodeContentId(domainMgr, domain, entry))
      else matcherGen.spawn(parsed, None, regexDict)(domainMgr, subMatchCheckerLib, Option(domain)).get
    }
    if (matchers.size == 1) matchers.head
    else matchersOR(matchers.toSeq)
  }

  sealed class AppliedPatternDomain(val domain:String, val appliedPatterns:List[AppliedPattern]) {
    private val _map:Map[String,AppliedPattern] = appliedPatterns.map(ap => (ap.ptnId, ap)).toMap
    private def get(ptnId:String):Option[AppliedPattern] = _map.get(ptnId)
    def applyPattern(domainMgr:LangDomainManager, groupId:String, matcherGen:TMatcherGen, ptn:TLangPattern, regexDict:Map[String,String], defaultMatchCheckerId:String)
                    (implicit subMatchCheckerLib: SubMatchCheckerLib):Seq[TMatcher] = {

      val ptnId: String = ptn.getId(groupId)
      val domainId = domain
      val domainPtnId = genId(domainId, ptnId)
      val appliedPattern = get(ptnId)
      if (appliedPattern.isEmpty) {
        val matchers = ptn.nodes.map(n => nodeContentMatcher(domainMgr, regexDict, matcherGen, domain, n))
        val negIndexes = ptn.nodes.indices.filter(idx => ptn.nodes(idx).neg)
        val matchCheckerIds = if (ptn.defaultLineTypes.nonEmpty) ptn.defaultLineTypes.toSeq else Seq(defaultMatchCheckerId)
        // the apply rule is not defined, which means default order and line type would be used
        Seq(
          matchersOrdered(matchers, negIndexes, matchCheckerIds, Option(domainPtnId))
          //if (matchCheckerIds.size == 1) matchersOrdered(matchers, negIndexes, matchCheckerIds.head, Option(domainPtnId))
          //else matchersOR(domainPtnId, matchCheckerIds.map(matchersOrdered(matchers, negIndexes, _)))
        )
      }
      else {
        val appPtn = appliedPattern.get
        val matchCheckerIds =
          if (appPtn.lineTypes.nonEmpty) appPtn.lineTypes.toSeq else {
            if (ptn.defaultLineTypes.nonEmpty) ptn.defaultLineTypes.toSeq
            else Seq(defaultMatchCheckerId)
          }
        //if (!ptn.languages.contains(application.lang)) throw new PatternLibErrorLangNotSupported(application.lang, ptnId)
        val nodesSet: IndexedSeq[List[PtnNode]] = if (appPtn.nodeOrders.isEmpty) IndexedSeq(ptn.nodes) else appPtn.nodeOrders.map(_.map(nodeId => ptn.getNode(nodeId))).toIndexedSeq
        val matcherLists: IndexedSeq[List[TMatcher]] = nodesSet.map(_.map(n => nodeContentMatcher(domainMgr, regexDict, matcherGen, domain, n)))
        if (matcherLists.size == 1) Seq(matchersOrderedAllPositive(matcherLists.head, matchCheckerIds, Option(domainPtnId))) //matchersOR(domainPtnId, matchCheckerIds.map(matchersOrderedAllPositive(matcherLists(0), _))))
        else {
          //val orderMatchers = matcherLists.indices.map(idx => matchersOR(genId(domainId, NodeOrder(ptnId, idx)), matchCheckerIds.map(matchersOrderedAllPositive(matcherLists(idx), _))))
          val orderMatchers = matcherLists.indices.map{ idx =>
            val id = genId(domainId, NodeOrder(ptnId, idx))
            matchersOrderedAllPositive(matcherLists(idx), matchCheckerIds, Option(id))
          }
          orderMatchers ++ Seq(matchersOR(domainPtnId, orderMatchers))
        }
      }
    }

    private def applyPatternGroup(lang:String, domainMgr:LangDomainManager,
                                  matcherGen:TMatcherGen, ptnGroup:LangPatternGroup,
                                  regexDict:Map[String,String],
                                  defaultMatchCheckerId:String)
                                 (implicit subMatchCheckerLib: SubMatchCheckerLib):Set[TMatcher] = {
      //val result = mutable.Set[TMatcher]()
      val filtered = ptnGroup.getPatterns.filter(_.supportedLanguages.contains(lang))
      val r = filtered.flatMap(applyPattern(domainMgr, ptnGroup.id, matcherGen, _, regexDict, defaultMatchCheckerId)).toSet
      val fullId = domainMgr.getFullId(domain, ptnGroup.id)
      //val r = result.toSet
      //r + matchersOR(ptnGroup.id, r.toSeq)
      //if (ptnGroup.patterns.size > 1) r + matchersOR(fullId, r.toSeq) else r
      if (r.isEmpty || r.exists(_.idEquals(fullId))) r // the pattern in the group doesn't have an id and uses the group id, in this case, we don't generate an OR matcher for the whole group
      else r + matchersOR(fullId, r.toSeq)
    }

    def applyPatternDomain(lang:String,
                           domainMgr:LangDomainManager,
                           matcherGen:TMatcherGen,
                           ptnDomain:LangPatternDomain,
                           ptnTemplateLib:LangPatternGroupTemplateLib,
                           regexDict:Map[String,String],
                           defaultMatchCheckerId:String)
                          (implicit subMatchCheckerLib: SubMatchCheckerLib):Set[TMatcher] = {
      val patternGroups = ptnDomain.instantiatePatternGroups(ptnTemplateLib, lang)
      val matchers = patternGroups.map(applyPatternGroup(lang, domainMgr, matcherGen, _, regexDict, defaultMatchCheckerId)) //++ ptnDomain.listMatchers
      matchers
    }
  }

  implicit def mergeMatcherSets(sets:Iterable[Set[TMatcher]]):Set[TMatcher] = sets.fold(Set())(_ ++ _)

  private val EmptyAppliedPatternList:List[AppliedPattern] = List()
  private def emptyAppliedPatternDomain(domainId:String):AppliedPatternDomain = new AppliedPatternDomain(domainId, EmptyAppliedPatternList)

  class AppliedPattern4Lang(val lang:String, val appliedPatternDomains:List[AppliedPatternDomain], val defaultMatchCheckerId:String) {
    private val _map:Map[String, AppliedPatternDomain] = appliedPatternDomains.map(ap => (ap.domain, ap)).toMap
    def get(domain:String):AppliedPatternDomain = _map.getOrElse(domain, emptyAppliedPatternDomain(domain))
    def applyDomain(domainMgr:LangDomainManager, matcherGen:TMatcherGen, ptnDomain:LangPatternDomain, ptnTemplateLib:LangPatternGroupTemplateLib, regexDict:Map[String,String])
                   (implicit subMatchCheckerLib: SubMatchCheckerLib):Set[TMatcher] =
      get(ptnDomain.id).applyPatternDomain(lang, domainMgr, matcherGen, ptnDomain, ptnTemplateLib, regexDict, defaultMatchCheckerId)
    def applyAllDomains(domainMgr:LangDomainManager, matcherGen:TMatcherGen, ptnTemplateLib:LangPatternGroupTemplateLib, regexDict:Map[String,String])
                       (implicit subMatchCheckerLib: SubMatchCheckerLib):Set[TMatcher] =
      domainMgr.langDomains.map(d => get(d.id).applyPatternDomain(lang, domainMgr, matcherGen, d, ptnTemplateLib, regexDict, defaultMatchCheckerId))
  }

  implicit val _formats = Serialization.formats(NoTypeHints)
  def fromJson(json:String):AppliedPattern4Lang = parse(json).extract[AppliedPattern4Lang]
}