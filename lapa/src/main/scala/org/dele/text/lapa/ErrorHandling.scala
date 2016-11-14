package org.dele.text.lapa

import org.dele.text.maen.ConfValueStringParser.Parsed
import org.dele.text.maen.utils.MaenError
import org.dele.text.maen.utils.MaenError.MaenErrorBase

/**
  * Created by jiaji on 2016-02-12.
  */
object ErrorHandling {


  final case class PatternErrorToDo(taskDesc:String)
    extends MaenErrorBase("[todo] %s".format(taskDesc))

  val PatternGroupJsonErrorUnknownType = new MaenErrorBase("Unknown pattern group type")

  final case class PatternGroupJsonErrorPatternOrTemplate(templateId:String)
    extends MaenErrorBase(s"Template [$templateId]: Either 'patterns' or 'template' is required")

  final case class DomainVarIdErrorIdNotFound(id:String)
    extends MaenErrorBase("Id [%s] not found!".format(id))

  final case class DomainVarIdErrorMultipleIdDefFound(id:String, domainIds:List[String])
    extends MaenErrorBase("Id [%s] defined in multiple domains [%s]!".format(id, domainIds.mkString(",")))

  final case class MatcherGenErrorUndefinedTemplate(templId:String)
    extends MaenErrorBase("Template with Id [%s] undefined!".format(templId))

  final case class MatcherGenErrorFailed(parsedDefi: Parsed)
    extends MaenErrorBase(s"Failed to generate matcher with definition $parsedDefi!")

  final case class PatternLibErrorPtnNotFound(ptnId:String)
    extends MaenErrorBase("Pattern [%s] not found!".format(ptnId))

  final case class DomainListDefsErrorDomainNotFound(domain:String)
    extends MaenErrorBase("Domain [%s] not defined!".format(domain))

  final case class PatternLibErrorLangNotSupported(lang:String, ptnId:String)
    extends MaenErrorBase("Language [%s] not supported for pattern [%s]!".format(lang, ptnId))
}
