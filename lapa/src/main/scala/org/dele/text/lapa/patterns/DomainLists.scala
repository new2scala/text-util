package org.dele.text.lapa.patterns

import org.dele.text.lapa.ErrorHandling.{DomainListDefsErrorDomainNotFound}
import DomainStructure.{LangDomainManager, Domain}
import TLangPattern.LangPatternDomain
import org.dele.text.maen.{AtomPropMatcherLib, ConfValueStringParser, TAtomMatcher}
import org.dele.text.maen.matchers.{SubMatchCheckerLib, TMatcher}
import org.json4s.NoTypeHints
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization

/**
  * Created by jiaji on 2016-02-21.
  */
object DomainLists {
  import TMatcher._
  import DomainStructure.genId

  //private def parseEntry(entry:String, matcherGen:TMatcherGen):TAtomMatcher = {
    //val (tmplId, tmplParams) = ConfValueStringParser.parse(entry)
    //AtomPropMatcherLib.spawn(tmplId, tmplParams)
    //matcherGen.spawn(entry, )
  //}

  sealed class ListDef(val id:String, val lines:List[List[String]]) {
    def getMatcher(domain:Domain, matcherGen:TMatcherGen, regexDict:Map[String,String])(implicit domainManager:LangDomainManager, subMatchCheckerLib: SubMatchCheckerLib):TMatcher = {
      val mid = genId(domain.id, id)

      implicit val domainId = Option(domain.id)
      if (lines.size == 1) ListNGram(lines.head.map(e => {
        val parsed = ConfValueStringParser.parse(e)
        matcherGen.spawn(parsed, None, regexDict).get
      }), Option(mid))
      else matchersOR(mid, lines.map(
        l => {
          ListNGram(l.map(e => {
            val parsed = ConfValueStringParser.parse(e)
            matcherGen.spawn(parsed, None, regexDict).get
          }))
        }
      ))
    }
  }

  sealed class DomainListDefs(val domain:String, val lists:List[ListDef]) {
    private val _map:Map[String,ListDef] = lists.map(l => (l.id, l)).toMap
    def getMatchers(matcherGen:TMatcherGen, regexDict:Map[String,String])(implicit domainManager:LangDomainManager, subMatchCheckerLib: SubMatchCheckerLib):Set[TMatcher] = {
      val d = domainManager.getDomain(domain)
      if (d.isEmpty) throw DomainListDefsErrorDomainNotFound(domain)
      else lists.map(_.getMatcher(d.get, matcherGen, regexDict)).toSet
    }
  }

  sealed class LangDomainLists(val lang:String, val domainLists:List[DomainListDefs]) {
    private val _map:Map[String,DomainListDefs] = domainLists.map(dl => (dl.domain, dl)).toMap
    def getMatchers(matcherGen:TMatcherGen, regexDict:Map[String,String])(implicit domainManager:LangDomainManager, subMatchCheckerLib: SubMatchCheckerLib):Set[TMatcher] =
      domainLists.map(_.getMatchers(matcherGen, regexDict)).foldLeft(Set[TMatcher]())(_ ++ _)
  }

  implicit val _formats = Serialization.formats(NoTypeHints)
  def loadDomainLists(json:String):LangDomainLists = parse(json).extract[LangDomainLists]
}
