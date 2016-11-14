package org.dele.text.lapa.patterns

import org.dele.text.lapa.ErrorHandling._
import org.dele.text.lapa.patterns.LangPatternGroupTemplate.LangPatternGroupTemplateLib
import org.dele.text.maen.ConfValueStringParser.Parsed
import org.dele.text.maen.utils.MaenError
import org.dele.text.maen.{AtomPropMatcherLib, ConfValueStringParser, TAtomMatcher}
import org.dele.text.maen.matchers.TMatcher
import org.dele.text.lapa.patterns.LangPatternGroupTemplate.LangPatternGroupTemplateLib
import org.json4s.JsonAST.{JNothing, JValue}
import org.json4s.{CustomSerializer, NoTypeHints, Serializer, TypeHints}
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization
import org.omg.CORBA.DomainManager

import scala.collection.mutable.ListBuffer


/**
  * Created by jiaji on 2016-02-12.
  */
trait TLangPattern {
  import TLangPattern._
  def getId(groupId:String):String
  def desc:String
  val languages:Set[String]
  def nodes:List[PtnNode]
  def defaultLineTypes:List[String]
  def getNode(nodeId:PNId):PtnNode
  //def addNode(node:PtnNode):Unit
}

object TLangPattern {
  type PNId = String

  class PtnNode(val id:PNId, val content:String, val isNeg:Option[Boolean]) {
    val contentEntries:List[(String,Parsed)] = parseNodeContent(content)
    override def toString:String = "%s:%s".format(id, content)
    //def
    private var _owner:TLangPattern = null
    def init(ptn:TLangPattern):Unit = _owner = ptn
    def owner = _owner
    def neg:Boolean = isNeg.nonEmpty && isNeg.get
  }

  def parseNodeContent(content:String):List[(String,Parsed)] = {
    val entries = content.split("\\s+")
    entries.map{ entry =>
      val parsed = ConfValueStringParser.parse(entry.trim)
      parsed.id -> parsed
    }.toList
  }

  //def createNode(nodeId:String, owner:TLangPattern, content:String):PtnNode = new PtnNode(owner, nodeId, content)

  import DomainStructure.{genId, IdTempl}
  private[patterns] class _LangPattern(val id:Option[String], val languages:Set[String], val nodes:List[PtnNode], val defLineTypes:List[String]) extends TLangPattern {
    def init(g:LangPatternGroup):Unit = {
      group = g
      nodes.foreach(_.init(this))
    }
    def supportedLanguages = if (languages.nonEmpty) languages else group.languages
    def defaultLineTypes = if (defLineTypes.nonEmpty) defLineTypes else group.defLineTypes
    def getId(groupId:String):String = if (id.isEmpty) groupId else IdTempl.format(groupId, id.get)
    def desc = nodes.indices.map(idx => (idx, nodes(idx))).map(p => "(%d)%s".format(p._1, p._2.content)).mkString(" ")
    private val _nodeMap:Map[PNId,PtnNode] = nodes.map(n => (n.id, n)).toMap
    def getNode(nodeId:PNId) =
      if (_nodeMap.contains(nodeId)) _nodeMap(nodeId) else throw PatternErrorToDo(s"Node id ($nodeId) not found!")
    var group:LangPatternGroup = null

    override def toString = nodes.map(_.toString).mkString(", ")
  }

  private def isExt(ext:Option[Boolean]) = ext.isDefined && ext.get
  abstract class LangPatternGroupJsonBase(val id:String, val languages:Set[String], val defLineTypes:List[String], val ext:Option[Boolean]) {
    //def toPatternGroup(implicit templateLib:LangPatternGroupTemplateLib)
    def isExtractPattern:Boolean = isExt(ext)
    def patternIds:Set[String]
  }

  //val JsonType_LangPatternGroup = "group"
  //val JsonType_LangPatternGroupTemplate = "template"
  //def createPtn(id:String, languages:Set[String]):TLangPattern = new _LangPattern(id, languages)
  sealed class LangPatternGroup private[TLangPattern](val id:String, val languages:Set[String], val patterns:List[_LangPattern], val defLineTypes:List[String], val ext:Option[Boolean]) {
    private lazy val _map:Map[String, _LangPattern] = patterns.map(p => (p.getId(id), p)).toMap
    def patternIds = _map.keySet
    def patternById(ptnId:String):Option[TLangPattern] = _map.get(ptnId)

    override def toString = id
    def getPatterns:List[_LangPattern] = patterns

    def isExtractPattern:Boolean = isExt(ext)
  }

  def createPatternGroup(id:String, languages:Set[String], patterns:List[_LangPattern], defLineTypes:List[String], ext:Option[Boolean]) = {
    val pg = new LangPatternGroup(id, languages, patterns, defLineTypes, ext)
    patterns.foreach(_.init(pg))
    pg
  }

  sealed class LangPatternGroupFromPatterns(id:String, languages:Set[String], val patterns:List[_LangPattern], defLineTypes:List[String], ext:Option[Boolean])
    extends LangPatternGroupJsonBase(id, languages, defLineTypes, ext) {
    def toPatternGroup:LangPatternGroup = createPatternGroup(id, languages, patterns, defLineTypes, ext)
    def patternIds:Set[String] = (id :: patterns.map(_.getId(id))).toSet
  }

  private val EmptyPatternIdSet = Set[String]()
  sealed class LangPatternGroupFromTemplate(id:String, languages:Set[String], private val defParsed:Parsed, defLineTypes:List[String], ext:Option[Boolean])
    extends LangPatternGroupJsonBase(id, languages, defLineTypes, ext) {
    def this(id:String, languages:Set[String], def2Parse:String, defLineTypes:List[String], ext:Option[Boolean]) = this(id, languages, ConfValueStringParser.parse(def2Parse), defLineTypes, ext)
    //private val defParsed = ConfValueStringParser.parse(template)
    def toPatternGroup(templateLib:LangPatternGroupTemplateLib, lang:String):Option[LangPatternGroup] = {
      if (languages.contains(lang)) {
        val pt = templateLib.byNameAndLang(defParsed.id, lang)
        pt.map(_.instantiate(id, defParsed.paras, defLineTypes, ext))
      }
      else None
      //new LangPatternGroup(id, languages, patterns, defLineTypes, ext)
    }
    def patternIds:Set[String] = Set(id) // child pattern ids are ignored
  }
/*
  private def parseEntry(entry:String):TAtomMatcher = {
    val parsed = ConfValueStringParser.parse(entry)
    AtomPropMatcherLib.spawn(parsed.id, parsed.paras)
  }
  */
/*
  import TMatcher._
  sealed class ListDef(val id:String, val lines:List[List[String]]) {
    def lineMatcher(domain:LangPatternDomain):TMatcher = {
      val mid = domain.genId(id)
      if (lines.size == 1) ListNGram(mid, lines.head.map(parseEntry))
      else matchersOR(mid, lines.map(
        l => {
          ListNGram(l.map(parseEntry))
        }
      ))
    }
  }
*/
  private def organizePatternGroupJsons(patternGroupJsons:List[LangPatternGroupJsonBase]):(List[LangPatternGroupFromPatterns], List[LangPatternGroupFromTemplate]) = {
    val patterns = ListBuffer[LangPatternGroupFromPatterns]()
    val templates = ListBuffer[LangPatternGroupFromTemplate]()
    patternGroupJsons.foreach{ _ match {
        case pattern:LangPatternGroupFromPatterns => patterns += pattern
        case template:LangPatternGroupFromTemplate => templates += template
        case x => throw PatternGroupJsonErrorUnknownType
      }
    }
    (patterns.toList, templates.toList)
  }

  val EmptyMatcherSet = Set[TMatcher]()
  sealed class LangPatternDomain(val id:String, val patternGroups:List[LangPatternGroupJsonBase]) {
    private[patterns] lazy val _map:Map[String, LangPatternGroupJsonBase] = patternGroups.map(pg => (pg.id, pg)).toMap
    def patternGroupJsonById(ptnGroupId:String):Option[LangPatternGroupJsonBase] = _map.get(ptnGroupId)

    private val (_patternGroupJsons, _patternGroupTemplateJsons) = organizePatternGroupJsons(patternGroups)
    def instantiatePatternGroups(templateLib:LangPatternGroupTemplateLib, lang:String):List[LangPatternGroup] = {
      _patternGroupJsons.map(_.toPatternGroup) ++ _patternGroupTemplateJsons.flatMap(_.toPatternGroup(templateLib, lang))
    }
    //def listMatchers:Set[TMatcher] = if (listDefs.isEmpty) EmptyMatcherSet else listDefs.get.map(_.lineMatcher(this)).toSet
  }
/*
  //private class LangPatternGroupSerializer extends Serializer[LangPatternGroup] {}
  object LangPatternGroupHints extends TypeHints {
    private val hintMap:Map[String,Class[_]] = Map( "group" -> classOf[LangPatternGroup], "template" -> classOf[LangPatternGroupFromTemplate])
    private val classMap = hintMap.map(_.swap)
    override val hints: List[Class[_]] = List(classOf[LangPatternGroup])
    override def classFor(hint:String):Option[Class[_]] = hintMap.get(hint)
    override def hintFor(clazz: Class[_]): String = classMap.get(clazz).get
    //override def hintFor(clazz: Class[_]):String =
  }
*/
  object LangPatternGroupSerializer extends CustomSerializer[LangPatternGroupJsonBase](format => (
    {
      case v:JValue => {
        val id = (v\"id").extract[String]
        val languages = (v\"languages").extract[Set[String]]
        val defLineTypes = (v\"defLineTypes").extract[List[String]]
        val _ext = (v\"ext")
        val ext = if (_ext != JNothing) Option(_ext.extract[Boolean]) else None

        val patterns = (v\"patterns")

        if (patterns != JNothing) {
          new LangPatternGroupFromPatterns(id, languages, patterns.extract[List[_LangPattern]], defLineTypes, ext)
        }
        else {
          val template = (v\"template")
          if (template != JNothing) {
            val params = (v\"params")
            if (params != JNothing) {
              val parsed = Parsed(template.extract[String], Array(params.extract[Array[String]]))
              new LangPatternGroupFromTemplate(id, languages, parsed, defLineTypes, ext)
            }
            else new LangPatternGroupFromTemplate(id, languages, template.extract[String], defLineTypes, ext)
          }
          else throw new PatternGroupJsonErrorPatternOrTemplate(id)
        }
      }
    },
    {
      case _ => throw MaenError.NotImplemented
    }
  ))

  implicit val _formats = Serialization.formats(NoTypeHints) + LangPatternGroupSerializer  //Serialization.formats(LangPatternGroupHints).withTypeHintFieldName("t")
  def parsePatternDomain(json:String):LangPatternDomain = {
    val domain = parse(json).extract[LangPatternDomain]
    //domain.patternGroups.foreach(_.init(domain))
    domain
  }
}