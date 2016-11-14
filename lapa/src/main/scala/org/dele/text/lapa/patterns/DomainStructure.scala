package org.dele.text.lapa.patterns

import org.dele.text.lapa.ErrorHandling
import org.dele.text.lapa.ErrorHandling.{DomainVarIdErrorIdNotFound, DomainVarIdErrorMultipleIdDefFound, PatternErrorToDo}
import org.dele.text.lapa.patterns.DomainLists.{DomainListDefs, LangDomainLists, ListDef}
import org.dele.text.lapa.patterns.TLangPattern.{LangPatternDomain, LangPatternGroup, LangPatternGroupJsonBase}
import org.dele.text.maen.matchers.{SubMatchCheckerLib, TMatcher}
import org.dele.text.lapa.patterns.DomainLists.{LangDomainLists, ListDef}
import org.dele.text.lapa.patterns.TLangPattern.{LangPatternDomain, LangPatternGroupJsonBase}
import org.json4s.NoTypeHints
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization

import scala.collection.mutable.ListBuffer

/**
  * Created by jiaji on 2016-02-19.
  */
object DomainStructure {
  val builtInGlobalDomainName = "_root_"
  val IdTempl = "%s.%s"
  def genId(domainId:String, elemId:String):String = IdTempl.format(domainId, elemId)
  def opGenId(domainId:Option[String], elemId:String):String = if (domainId.isDefined) genId(domainId.get, elemId) else elemId

  private[DomainStructure] sealed class _DomainRel(val id:String, val childIds:Option[List[String]], val lists:Option[List[String]])

  implicit val _formats = Serialization.formats(NoTypeHints)
  private[DomainStructure] def loadDomainRels(json:String):List[_DomainRel] = parse(json).extract[List[_DomainRel]]

  sealed class PatternDomainTree private[DomainStructure](val id:String, _children:List[PatternDomainTree]) {
    private val _ch:ListBuffer[PatternDomainTree] = {
      val ch = ListBuffer[PatternDomainTree]()
      ch ++= _children
      ch
    }

    def children = _ch.toList

    def removeDomains(toRemove:List[String]):Unit = {
      val ch2remove = _ch.filter(c => toRemove.contains(c.id))
      ch2remove.foreach(_ch -= _)
      _ch.foreach(_.removeDomains(toRemove))
    }

    private[DomainStructure] def offsprings:List[PatternDomainTree] = this :: children.flatMap(t => t.offsprings)
  }

  import scala.collection.mutable
  private val EmptyChildIdList= List[String]()
  private val EmptyDomainListSet= Set[String]()
  val RootNodeId = builtInGlobalDomainName
  def load(json:String, excludedDomains:List[String]):PatternDomainTree = {
    val domainRels = loadDomainRels(json)
    val drMap:Map[String,_DomainRel] =
      domainRels.map(dr => (dr.id, dr)).toMap
    val topDomainMap = mutable.Map[String, PatternDomainTree]()
    val allDomainMap = mutable.Map[String, PatternDomainTree]()
    domainRels.foreach{ dr =>
      if (!allDomainMap.contains(dr.id)) {
        val subTree = loadRec(drMap, dr)
        topDomainMap += dr.id -> subTree
        allDomainMap ++= subTree.offsprings.map(t => t.id -> t)
      }
    }

    val root = topDomainMap.values.head
    root.removeDomains(excludedDomains)
    root
    //new PatternDomainTree(RootNodeId, topDomainMap.values.toList)
  }

  private val EmptyChildrenList = List[PatternDomainTree]()
  private def loadRec(drMap:Map[String,_DomainRel], dr:_DomainRel): PatternDomainTree = {
    val childIds = drMap.get(dr.id)
    //val lists = if (dr.lists.isEmpty) EmptyDomainListSet else dr.lists.get.toSet
    if (childIds.isEmpty) new PatternDomainTree(dr.id, EmptyChildrenList)
    else {
      val children = if (dr.childIds.isEmpty) EmptyChildrenList
      else dr.childIds.get.map(
        cid => {
          val cdr = drMap.get(cid)
          if (cdr.isEmpty) new PatternDomainTree(cid, EmptyChildrenList)
          else loadRec(drMap, cdr.get)
        }
      )
      new PatternDomainTree(dr.id, children)
    }
  }

  class Domain(val id:String, val patternGroupDefs:List[LangPatternGroupJsonBase], val lists:List[ListDef]) {
    override def toString = """%s: patterns(%d) lists(%d)""".format(id, patternGroupDefs.size, lists.size)
  }

  def getAllDomains(domain:PatternDomainTree):Set[PatternDomainTree] = domain.children.map(_.offsprings).foldLeft(Set[PatternDomainTree]())(_ ++ _) + new PatternDomainTree(builtInGlobalDomainName, List())

  val EmptyPatternGroups = List[LangPatternGroupJsonBase]()
  val EmptyLists = List[ListDef]()
  def domainMap(domainStructure:PatternDomainTree, langDomains:List[LangPatternDomain], domainLists:LangDomainLists):Map[String,Domain] = {
    val allDomains:Set[PatternDomainTree] = getAllDomains(domainStructure)
    val domainsWithPatterns = langDomains.map(ld => (ld.id, ld)).toMap
    val domainsWithLists = domainLists.domainLists.map(dl => (dl.domain, dl)).toMap
    allDomains.map(ad => {
      val patterns = if (domainsWithPatterns.contains(ad.id)) domainsWithPatterns.get(ad.id).get.patternGroups else EmptyPatternGroups
      val lists = if (domainsWithLists.contains(ad.id)) domainsWithLists.get(ad.id).get.lists else EmptyLists
      (ad.id, new Domain(ad.id, patterns, lists))
    }).toMap
  }

  sealed class LangDomainManager(val domainStructure:PatternDomainTree,
                                 val langDomains:List[LangPatternDomain],
                                 domainLists:LangDomainLists,
                                 subMatchCheckerLib:SubMatchCheckerLib)
                                //(implicit exGlobalDomainIds:Map[String,List[String]] )
  {
    val globalDomainIds = List(builtInGlobalDomainName) //(exGlobalDomainIds.keySet + builtInGlobalDomainName).toList //if (optionalGlobalDomainIds.isDefined) builtInGlobalDomainName :: optionalGlobalDomainIds.get else List(builtInGlobalDomainName)
    private val _domainMap = domainMap(domainStructure, langDomains, domainLists)
    def getDomain(id:String):Option[Domain] = _domainMap.get(id)
    def listMatchers(matcherGen:TMatcherGen, regexDict:Map[String,String]):Set[TMatcher] = domainLists.getMatchers(matcherGen, regexDict)(this, subMatchCheckerLib)

    def getChildDomainNames(id:String):List[String] = child2RootDomains.filter(_._2 == id).map(_._1).toList

    private val rootDomainIds:List[String] = domainStructure.children.map(_.id) ::: globalDomainIds
    private val rootDomains:List[Domain] = rootDomainIds.flatMap(_domainMap.get)
    private def getDomainIds(domainId:String):(String,Set[String]) = {
      val domain = langDomains.filter(_.id == domainId).headOption
      val s1 = if (domain.nonEmpty) {
        val patternGroupIds = domain.get.patternGroups.map(_.id)
        val patternIds = domain.get.patternGroups.flatMap(_.patternIds)
        (patternGroupIds ++ patternIds).toSet
      } else Set[String]()

      val listSet = domainLists.domainLists.filter(_.domain == domainId).headOption
      val s = if (listSet.nonEmpty) s1 ++ listSet.get.lists.map(_.id) else s1
      domainId -> s
    }
    private def getId2DomainMap(domainIds:List[String]):Map[String,String] = {
      val filteredDomains = langDomains.filter(ld => domainIds.contains(ld.id))
      val filteredDomainLists = domainLists.domainLists.filter(dl => domainIds.contains(dl.domain))
      filteredDomains.flatMap(
        ld => {
          val patternGroupIds = ld.patternGroups.map(_.id)
          val patternIds = ld.patternGroups.flatMap(_.patternIds)
          (patternGroupIds ++ patternIds).map(pid => (pid, ld.id))
        }
      ).toMap ++
      filteredDomainLists.flatMap(
        dl => dl.lists.map(l => (l.id, dl.domain))
      ).toMap
    }
    /*
    private val exGlobalIdMaps:Map[String,Map[String,String]] = exGlobalDomainIds.map(
      p => {
        val ids = p._2
        p._1 -> p._2.map(_ -> p._1).toMap
      }
    )
    */
    private val rootDomainIdMaps:Map[String,Map[String,String]] = {
      domainStructure.children.map(
        rootDomain => {
          val subDomainIds = rootDomain.offsprings.map(_.id)
          (rootDomain.id, getId2DomainMap(subDomainIds))
        }
      ).toMap ++ globalDomainIds.map(gid => (gid, getId2DomainMap(List(gid)))) // ++ exGlobalIdMaps.getOrElse(gid, Map())
    }
    private val child2RootDomains:Map[String,String] = {
      domainStructure.children.flatMap(
        root => root.offsprings.map(oid => (oid.id, root.id))
      ).toMap
    }

    //val LAGlobalDomain = ""

    def queryGlobalDomainId(id:String):Option[String] = {
      val definedIn = globalDomainIds.filter(gid => rootDomainIdMaps.get(gid).get.get(id).isDefined)
      if (definedIn.size == 1) Option(definedIn(0))
      else if (definedIn.size > 1) throw DomainVarIdErrorMultipleIdDefFound(id, definedIn)
      else None //throw DomainVarIdErrorIdNotFound(id)
    }

    def getGlobalDomainFullId(id:String):String = opGenId(queryGlobalDomainId(id), id)

    private def child2ParentMaps(p:PatternDomainTree):Map[String,String] = {
      val m1 = p.children.map(c => c.id -> p.id).toMap
      if (p.children.nonEmpty) {
        val m2 = p.children.map(child2ParentMaps).reduce(_ ++ _)
        m1 ++ m2
      }
      else m1
    }

    private val child2ParentDomains:Map[String,String] = child2ParentMaps(domainStructure)
    private val allDomainIds = domainStructure.offsprings.map(_.id)
    private val domain2IdSets:Map[String,Set[String]] = allDomainIds.map(getDomainIds).toMap
    def queryDomainId(currDomain:String, id:String):Option[String] = {
      import scala.util.control.Breaks._

      var d = Option(currDomain)
      breakable {
        while(d.nonEmpty) {
          val domain = if (_domainMap.contains(d.get)) _domainMap(d.get)
            else throw PatternErrorToDo(s"$d not found in domain map!")
          if (domain2IdSets.contains(domain.id)) {
            if (domain2IdSets.get(domain.id).get.contains(id)) break
          }
          d = child2ParentDomains.get(d.get)
        }
      }
      d
      /*
      val rootDomainId = child2RootDomains.get(currDomain)
      if (rootDomainId.isEmpty) {
        // todo: error checking
        throw ErrorHandling.NotImplemented
      }
      val domainId = rootDomainIdMaps.get(rootDomainId.get).get.get(id)
      if (domainId.isDefined) domainId
      else queryGlobalDomainId(id)
      */
    }
    def getFullId(currDomain:String, id:String) = opGenId(queryDomainId(currDomain, id), id)
  }
}
