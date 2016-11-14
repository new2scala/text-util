package org.dele.text.maen.extracts

import org.dele.text.maen.utils.HamletError
import org.dele.text.maen.{AtomSeqMatch, ConfValueStringParser, TAtomMatcher, TMatchResultPool}
import org.dele.text.maen.matchers.{SubMatchCheckerLib, TMatcher, TSubMatchChecker}
import org.dele.text.maen.matchers.TMatcher.MId
import org.dele.text.maen.matchers.{SubMatchCheckerLib, TMatcher}
import org.dele.text.maen.utils.HamletError
import org.dele.text.maen.{AtomSeqMatch, ConfValueStringParser, TAtomMatcher, TMatchResultPool}
import org.json4s.JsonAST.{JArray, JField, JObject, JString}
import org.json4s.{CustomSerializer, NoTypeHints}
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization

import scala.collection.mutable.ListBuffer

/**
  * Created by jiaji on 2016-02-12.
  */
class Extract private (
  val name:String,
  val duplCheckable:Boolean,
  val instances:List[AtomSeqMatch]
) {
  /*
  lazy val range = {
    val ranges = instances.map(_.range)
    val min = ranges.map(_.start).min
    val max = ranges.map(_.end).max
    min to max
  }
  */
  override def toString = "%s=[%s]".format(name, instances.mkString(", "))
}

object Extract {

  import org.dele.text.maen.AtomPropMatcherLib.{E, Er, TmplE, TmplEr}
  import org.dele.text.maen.ErrorHandling._

  private def filterByBlockerMatches(rawAllMatches:List[AtomSeqMatch], blockerMids:List[MId], resultPool: TMatchResultPool) = {
    val blockerMatches:List[AtomSeqMatch] = blockerMids.flatMap(resultPool.query)
    rawAllMatches.filter{ rm => !blockerMatches.exists(_.isOverlap(rm)) }
  }

  abstract class ExtractDefBase(val extractName:String, val duplCheckable:Boolean, protected val matcherId:Option[MId]) {
    protected def getInstances(input:AtomSeqMatch):List[AtomSeqMatch]
    def process(input:AtomSeqMatch):Extract = {
      val instances = getInstances(input)

      new Extract(extractName, duplCheckable, instances)
    }
    def id = "%s.%s".format(matcherId, extractName)
    override def toString = "[%s -> %s]".format(matcherId, extractName)
    var domain:ExtractDomain = null
    def init(d:ExtractDomain):Unit = domain = d
    //def fullMatcherId = if (domain == null) matcherId else "%s.%s".format(domain.domain, matcherId)
  }

  private val EmptyRegexDict = Map[String,String]()

  private def AtomMatcherFromTmplId(tmplId:String, param:Array[String]):TAtomMatcher = {
    tmplId match {
      case TmplE => E(EmptyRegexDict, param)
      case TmplEr => Er(EmptyRegexDict, param)
      case _ => throw HamletError.NotImplemented
    }
  }


  private[Extract] class _PropExtractDef(extractName:String, matcherId:MId, val matcherTmplId:String, val tmplParams:List[Array[String]])
    extends ExtractDefBase(extractName, true, Option(matcherId)) {
    //private val (matcherTmplId, tmplParams) = AtomPropMatcherParser.parseTmplId(atomMatcherDef)
    def getInstances(input:AtomSeqMatch):List[AtomSeqMatch] = {

      val propMatcher = AtomMatcherFromTmplId(matcherTmplId, tmplParams.head)
      val qualifiedSubMatches:List[AtomSeqMatch] = input.allSubMatches(_.matcher.idEquals(matcherId))
      val singleAtomSubMatches = qualifiedSubMatches.flatMap(_.allSubMatches(_.range.length == 1))
      // only return sub-matches that doesn't have any sub-matches, i.e. leaf matches
      singleAtomSubMatches.filter(sm => sm.subMatches.isEmpty && propMatcher.check(sm.atoms(0)))
    }
  }

  private[Extract] class _EntityTypeExtractDef(extractName:String, matcherId:MId, val entityTypes:Array[String])
    extends _PropExtractDef(extractName, matcherId, TmplE, List(entityTypes))

  def extractEntities(extractName:String, matcherId:MId, entityTypes:String*):ExtractDefBase =
    new _EntityTypeExtractDef(extractName, matcherId, entityTypes.toArray)


  def extractByAtomMatcherDef(extractName:String, matcherId:MId, atomMatcherDef:String):ExtractDefBase = {
    val parsed = ConfValueStringParser.parse(atomMatcherDef)
    if (parsed.id == TmplE) new _EntityTypeExtractDef(extractName, matcherId, parsed.paras(0))
    else new _PropExtractDef(extractName, matcherId, parsed.id, parsed.paras.toList)
  }

  import org.dele.text.maen.matchers.TMatcher._

  private def idMatch(input:AtomSeqMatch, mid:MId) = input.matcher != null && input.matcher.id.nonEmpty && input.matcher.id.get == mid
  private[Extract] class _ExtractWholeDef(extractName:String, matcherId:Option[MId])
    extends ExtractDefBase(extractName, false, matcherId) {
    /*
    def getInstances(input:AtomSeqMatch):Seq[AtomSeqMatch] = {
      input.allSubMatches(subMatch => subMatch.matcher != null && !subMatch.matcher.id.isEmpty && subMatch.matcher.id.get == matcherId)
    }
    */
    def getInstances(input:AtomSeqMatch):List[AtomSeqMatch] = {
      if (matcherId.isEmpty) List(input)
      else {
        val subMatches = input.allSubMatches(idMatch(_, matcherId.get))
        //if (idMatch(input, matcherId.get)) input :: subMatches
        subMatches
      }
    }
  }

  def extractWhole(extractName:String, matcherId:Option[String]):ExtractDefBase = new _ExtractWholeDef(extractName, matcherId)

  class RelatedEntityExtracts(val name:String, val atomMatcherDefs:List[String], val blockers:List[String])

  val EmptyRelatedEntityCheckerIds = List()

  import collection.mutable
  private val BlockerSplitter = "\\,"
  private def parseBlockConf(extractBlocks:List[String]):(Map[String,Set[String]], Map[String, Set[String]]) = {
    val extrBlockMap = mutable.Map[String,Set[String]]()
    val matchBlockMap = mutable.Map[String,Set[String]]()
    extractBlocks.map{ eb =>
      val parts = eb.split("\\:").map(_.trim)
      if (parts.length != 2) throw ExtractBlockErrorIllFormattedDef(eb)
      else {
        val part1Parts = parts(1).split("\\|").map(_.trim)
        if (part1Parts(0).nonEmpty) {
          extrBlockMap(parts(0)) = part1Parts(0).split(BlockerSplitter).toSet
        }
        if (part1Parts.size >= 2 && part1Parts(1).nonEmpty) {
          matchBlockMap(parts(0)) = part1Parts(1).split(BlockerSplitter).toSet
        }
      }
    }
    (extrBlockMap.toMap, matchBlockMap.toMap)
  }

  private val EmptyRelEntMatchers = List[TMatcher]()
  class ExtractDomain(val domain:String, val nonEventExtracts:Set[String], val relEntExtracts:Option[RelatedEntityExtracts], val extractBlocks:List[String], extractDefs:List[ExtractDefBase]) {

    private val (_extractBlockMap, _matchBlockMap):(Map[String,Set[String]], Map[String, Set[String]]) = parseBlockConf(extractBlocks)
    def hasBlocker(exAttrName:String):Boolean = _extractBlockMap.contains(exAttrName)
    private val _map:Map[String, List[ExtractDefBase]] = {
      extractDefs.foreach(_.init(this))
      extractDefs.groupBy(_.extractName).map(p => (p._1, p._2))
    }

    private def getRelEntMatchers(implicit subMatchCheckerLib:SubMatchCheckerLib):List[TMatcher] = {
      if (relEntExtracts.isEmpty) EmptyRelEntMatchers
      else {
        val exName = relEntExtracts.get.name
        relEntExtracts.get.atomMatcherDefs.map { d =>
          val parsed = ConfValueStringParser.parse(d)
          fromAtomMatcher(AtomMatcherFromTmplId(parsed.id, parsed.paras(0)))
        }
      }
    }

    private def extractRelEnt(m:AtomSeqMatch, subMatchCheckerIds:Iterable[String]):Option[Extract] = {
      if (relEntExtracts.isEmpty) None
      else {
        implicit val submatchCheckerLib = m.resultPool.subMatchCheckerLib
        val relEntMatchers = getRelEntMatchers
        val rawAllMatches = relEntMatchers.map(_.m(m.resultPool)).reduce(_ ++ _)
        //val blockerMatches:List[AtomSeqMatch] = relEntExtracts.get.blockers.flatMap(m.resultPool.query)
        //val allMatches = rawAllMatches.filter{ rm => !blockerMatches.exists(_.isOverlap(rm)) }
        val allMatches = filterByBlockerMatches(rawAllMatches.toList, relEntExtracts.get.blockers, m.resultPool)
        if (allMatches.nonEmpty) {
          val checked = if (subMatchCheckerIds.nonEmpty) {
            val smCheckers = subMatchCheckerIds.map(m.resultPool.getSubMatchChecker)
            allMatches.filter{ am =>
              val toCheck = Seq(am, m).sortBy(_.range.start)
              smCheckers.exists(_.check(toCheck, m.resultPool))
            }
          }
          else allMatches
          Option(new Extract(relEntExtracts.get.name, true, checked.toList))
        }
        else None
      }
    }

    private def mergeInstances(matches:List[AtomSeqMatch]):List[AtomSeqMatch] = {
      val sorted = matches.sortBy(m => m.range.start -> m.range.end)
      val r = ListBuffer[AtomSeqMatch]()
      sorted.foreach{ m =>
        if (notFoundIn(m, r.toSet)) r += m
      }
      r.toList
    }

    //def getExtractDef(id:String):List[ExtractDefBase] = _map.get(id).get
    def run(m:AtomSeqMatch, relEntSubMatchCheckerIds:Iterable[String]):Map[String,Extract] = {
      val raw0 = _map.map(p => {

        val extracts = p._2.map(_.process(m))
        val allInstances = extracts.foldLeft(List[AtomSeqMatch]())(_ ++ _.instances)
        val mergedExtracts = mergeInstances(allInstances)

        val blockers = _matchBlockMap.getOrElse(p._1, EmptyIdSet)
        val filtered = filterByBlockerMatches(mergedExtracts, blockers.toList, m.resultPool)
        val duplCheckable = if (extracts.nonEmpty) extracts.head.duplCheckable else true
        p._1 -> new Extract(p._1, duplCheckable, filtered)
        //new Extract(p._1, mergedExtract)
      })
      val nonEvtExts = nonEventExtracts.flatMap(raw0.get).map(_.instances)
      val isNonEvt =  nonEvtExts.exists(_.nonEmpty)
      val raw = if (isNonEvt) raw0 // do not extract related entities for nonEventExtracts
        else {
          val relEntExt = extractRelEnt(m, relEntSubMatchCheckerIds)
          if (relEntExt.nonEmpty) raw0 + (relEntExt.get.name -> relEntExt.get) else raw0
        }
      val toCheckBlock = raw.filter(exp => _extractBlockMap.contains(exp._1))
      val noNeedToCheck = raw.filter(ex => !_extractBlockMap.contains(ex._1))
      val rawAll = raw.map(_._2)
      val checkResult:Iterable[Extract] = toCheckBlock.flatMap(tc => checkUnblockedExtract(tc._2, rawAll))
      /*
      toCheckBlock.foreach{ b =>
        val blockingExtractNames = _blockMap(b._1)
        val blockingExtractInsts = raw.filter(exp => blockingExtractNames.contains(exp._1)).flatMap(_._2.instances).toSet
        val unblockedInsts = b._2.instances.filter(!blockingExtractInsts.contains(_))
        if (unblockedInsts.nonEmpty) checkResult += new Extract(b._1, unblockedInsts)
      }
      */
      noNeedToCheck ++ checkResult.map(r => r.name -> r)
    }

    def checkUnblockedExtract(ex:Extract, all:Iterable[Extract]):Option[Extract] = {
      val blockingExtractNames = _extractBlockMap(ex.name)
      val blockingExtractInsts = all.filter(ex => blockingExtractNames.contains(ex.name)).flatMap(_.instances).toSet
      // check ranges only since AtomSeqMatch equality checking checks matcher Id too
      //val blockingInstRanges = blockingExtractInsts.map(_.range)
      val unblockedInsts = ex.instances.filter(i => notFoundIn(i, blockingExtractInsts))

      if (unblockedInsts.isEmpty) None
      else Option(new Extract(ex.name, ex.duplCheckable, unblockedInsts))
    }
  }

  def notFoundIn(blockee:AtomSeqMatch, blockers:Set[AtomSeqMatch]):Boolean = {
    val notBlockedByRange = !blockers.map(_.range).contains(blockee.range)
    if (notBlockedByRange) {
      // check text, because in many cases, sentence contains duplicate text segments.
      val blockerTexts = blockers.map(_.atoms.map(_.text))
      val blockeeText = blockee.atoms.map(_.text)
      !blockerTexts.contains(blockeeText)
    }
    else false
  }


  class ExtractDefLib(val extractDefSets:List[ExtractDomain]) {
    private val _map:Map[String, ExtractDomain] = extractDefSets.map(eds => (eds.domain, eds)).toMap
    def getExtractDefSet(tag:String):ExtractDomain = _map.get(tag).get
  }

  class ExtractDefSerializer extends CustomSerializer[ExtractDefBase](
    format => (
      {
        case JObject(List(
          JField("extractName", JString(extractName)),
          JField("matcherId", JString(matcherId)),
          JField("atomMatcherDef", JString(atomMatcherDef))
          //JField("tmplParams", tmplParams)
        )) => {
          /*
          val param:Array[Array[String]] = tmplParams match {
            case JString(str) => Array(Array(str))
            case JArray(strLst) => strLst match {
              case strs:List[JString] => Array(strs.map(_.extract[String]).toArray)
              case y => throw NotImplemented
            }
            case x => throw NotImplemented
          }
          new _PropExtractDef(extractName, matcherId, matcherTmplId, param)
          */
          //throw NotImplemented
          extractByAtomMatcherDef(extractName, matcherId, atomMatcherDef)
        }
        case JObject(List(
          JField("extractName", JString(extractName)),
          JField("matcherId", JString(matcherId))
        )) => new _ExtractWholeDef(extractName, Option(matcherId))
        case JObject(List(JField("extractName", JString(extractName)))) => new _ExtractWholeDef(extractName, None)
        case err => throw ExtractErrorJSONFormat(err)
      },
      {
        case _ => throw HamletError.NotImplemented
      }
      )
  )

  implicit val _formats = Serialization.formats(NoTypeHints) + new ExtractDefSerializer
  def fromJson(json:String):ExtractDefLib = parse(json).extract[ExtractDefLib]
}