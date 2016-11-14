package org.dele.text.lapa.patterns

import org.dele.text.lapa.ErrorHandling.{MatcherGenErrorFailed, MatcherGenErrorUndefinedTemplate}
import DomainStructure.LangDomainManager
import org.dele.text.maen.ConfValueStringParser.Parsed
import org.dele.text.maen.matchers.MatcherTmpl.{DomainIdFinder, MatcherTmplLib}
import org.dele.text.maen.matchers.SubMatchCheckerLib._
import org.dele.text.maen.matchers.{MatcherTmpl, MatcherManager, SubMatchCheckerLib, TMatcher}
import org.dele.text.maen.{ConfValueStringParser, AtomPropMatcherLib}
import org.dele.text.maen.matchers.TMatcher.MId

import scala.util.{Failure, Success, Try}

/**
  * Created by jiaji on 2016-03-01.
  */
trait TMatcherGen {
  def spawn(parsedDefi: Parsed, id:Option[MId], regexDict:Map[String,String])(implicit domainManager:LangDomainManager, subMatchCheckerLib: SubMatchCheckerLib, domain:Option[String]):Try[TMatcher]
}

object TMatcherGen {

  private[TMatcherGen] class AtomLibMatcherGen extends TMatcherGen {
    import TMatcher._
    def spawn(parsedDefi: Parsed, id:Option[MId], regexDict:Map[String,String])(implicit domainManager:LangDomainManager, subMatchCheckerLib: SubMatchCheckerLib, domain:Option[String]):Try[TMatcher] = {
      //val parsed = ConfValueStringParser.parse(defi)
      if (AtomPropMatcherLib.contains(parsedDefi.id)) Success(fromAtomMatcher(AtomPropMatcherLib.spawn(parsedDefi.id, parsedDefi.paras, regexDict), EmptyCheckerIds, id))
      else Failure(MatcherGenErrorUndefinedTemplate(parsedDefi.id))
    }
  }

  val NoMatcherTemplateLibGen:TMatcherGen = new AtomLibMatcherGen

  private[TMatcherGen] class MatcherTemplateGen(val tmplLib:MatcherTmplLib) extends TMatcherGen {
    def spawn(parsedDefi: Parsed, id:Option[MId], regexDict:Map[String,String])(implicit domainManager:LangDomainManager, subMatchCheckerLib: SubMatchCheckerLib, domain:Option[String]):Try[TMatcher] = {
      if (tmplLib.contains(parsedDefi.id)) {
        val domainIdFinder:DomainIdFinder = id => if (domain.nonEmpty) domainManager.getFullId(domain.get, id) else domainManager.getGlobalDomainFullId(id)
        Success(tmplLib.spawn(parsedDefi.id, parsedDefi.paras, regexDict, id, Option(domainIdFinder)))
      }
      else Failure(MatcherGenErrorUndefinedTemplate(parsedDefi.id))
    }
  }

  private[TMatcherGen] class ChainedGen(val matcherGens:TMatcherGen*) extends TMatcherGen {
    def spawn(parsedDefi: Parsed, id:Option[MId], regexDict:Map[String,String])(implicit domainManager:LangDomainManager, subMatchCheckerLib: SubMatchCheckerLib, domain:Option[String]):Try[TMatcher] = {
      val gened = matcherGens.map(_.spawn(parsedDefi, id, regexDict))
      val filtered = gened.filter(_.isSuccess)
      if (filtered.nonEmpty) filtered(0)
      else Failure(MatcherGenErrorFailed(parsedDefi))
    }
  }

  private def TemplateGen(tmplLib:MatcherTmplLib)(implicit subMatchCheckerLib: SubMatchCheckerLib):TMatcherGen = {
    new MatcherTemplateGen(tmplLib)
  }

  def All(tmplLib:MatcherTmplLib)(implicit subMatchCheckerLib: SubMatchCheckerLib):TMatcherGen =
    new ChainedGen(TemplateGen(tmplLib), NoMatcherTemplateLibGen)
}