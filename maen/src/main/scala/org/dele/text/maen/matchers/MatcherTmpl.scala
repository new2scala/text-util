package org.dele.text.maen.matchers

import org.dele.text.maen.matchers.SubMatchCheckerLib._
import org.dele.text.maen.matchers.TMatcher._
import org.dele.text.maen.utils.HamletError
import org.dele.text.maen.{AtomPropMatcherLib, ConfValueStringParser}
import org.dele.text.maen.matchers.TSubMatchChecker._
import org.dele.text.maen.{AtomPropMatcherLib, ConfValueStringParser}

/**
  * Created by jiaji on 2016-02-29.
  */
import org.dele.text.maen.utils.HamletError._
object MatcherTmpl {
  //import scala.collection.mutable
  import TMatcher._
  import org.dele.text.maen.ErrorHandling._
  import scala.collection.mutable

  class MatcherTmplDef(val id:String, val t:String, val checkerIds:String) {
    def getCheckerIds = checkerIds.split("\\|")
  }

  private def paramIndexMap(param: Array[Array[String]]):Map[Int,Set[Int]] = {
    val r = mutable.Map[Int,Set[Int]]()

    param.indices.foreach{ x =>

      // arguments are supposed to be the first (and the only one) entry
      val paramIndex = ConfValueStringParser.parseParam(param(x)(0))
      if (paramIndex.nonEmpty) {
        if (!r.contains(paramIndex.get)) r(paramIndex.get) = Set(x)
        else r(paramIndex.get) = r(paramIndex.get) + x
      }
    }

    //if (r.exists(p => p._1 > paramCount || p._1 <= 0)) throw Todo("exception: param index out of range [1..size]")
    r.toMap
  }

  private def replaceParams(in: Array[Array[String]], paramIndexMap:Map[Int,Set[Int]], defiParams: Array[Array[String]]) = {
    /*
    val result = param.map(_.clone)
    in.indices.foreach(
      idx => {
        val coord = paramIndexMap.get(idx+1).get
        result(coord._1)(coord._2) = in(idx)
      }
    )
    result
    */
    // each parameter in 'in' will further be expanded into an array of parameters, separated by '/'
    val result = defiParams.map(_.clone)
    in.indices.foreach{ idx =>
      //val params = in(idx).split(ConfValueStringParser.SecondSeparatorStr).map(_.trim.replace(ConfValueStringParser.EscapedSecondSeparator, ConfValueStringParser.SecondSeparatorChar))
      val mapKey = idx + 1
      if (paramIndexMap.contains(mapKey)) {
        val resultIndex = paramIndexMap(mapKey)
        resultIndex.foreach(ri => result(ri) = in(idx))
      }
    }
    result
  }

  class MatcherFuncDef(val id:String, private val defi:Array[String]) {
    // note: all matcher functions do not use 2-d arguments, i.e. the definition looks like MatchFuncName(a1|a2)
    //   when the functions are applied, 2-d parameters are allowed by using '/' as parameter separator, e.g. MatchFuncName(p11/p12|p2)
    private lazy val _parsedMap = defi.indices.map(idx => idx -> ConfValueStringParser.parse(defi(idx))).toMap
    private lazy val _paramsMap = _parsedMap.map(p => p._1 -> p._2.paras)

    private lazy val _paramIndexMap:Map[Int, Map[Int,Set[Int]]] = _paramsMap.map(p => p._1 -> paramIndexMap(p._2))
    def paramCount = _paramIndexMap.map(_._2.size).max
    val templateCount = defi.length

    def templateId(idx:Int) = _parsedMap(idx).id

    def getParams(idx:Int, in:Array[Array[String]]):Array[Array[String]] = {
      if (in.length != paramCount) {
        throw Todo("exception: param count differ")
      }
      replaceParams(in, _paramIndexMap(idx), _paramsMap(idx))
    }
  }

  type DomainIdFinder = MId => MId
  def getDomainId(domainIdFinder: DomainIdFinder, id:MId) = domainIdFinder(id)
  def getDomainIds(domainIdFinder: DomainIdFinder, ids:Array[MId]):Array[MId] = ids.map(domainIdFinder)

  private def paramCountError(expCount:Int):String = s"Exact $expCount params required"
  private def paramMinCountError(min:Int):String = s"At least $min params required"
  private def SeqMatchers(funcId:String, params:Array[Array[String]], checkerIds:Array[String], id:Option[MId])
                         (implicit matchCheckerLib:SubMatchCheckerLib):TMatcher = {
    if (params.length < 2) throw MatcherTemplateErrorSpawnMatcher(funcId, paramMinCountError(2))
    val seqMatchers = params.map { p => queryPoolMatcher(p.toSet) }
    matchersOrderedAllPositive(seqMatchers, checkerIds, id)
  }

  val NABParamCount = 2
  private def NABMatchers(funcId:String, rawParams:Array[Array[String]], checkerIds:Array[String], flipOrder:Boolean, id:Option[MId])
                         (implicit matchCheckerLib:SubMatchCheckerLib):TMatcher = {
    if (rawParams.exists(_.length != 1)) throw MatcherTemplateErrorSpawnMatcher(funcId, s"cannot use OR('/') operator in NAB matchers")
    val params = rawParams.map(_(0)) // take only one
    if (params.length != NABParamCount) throw MatcherTemplateErrorSpawnMatcher(funcId, s"exact 2 params required")
    val (notMatcherId, matcherId) = if (!flipOrder) (params(0), params(1)) else (params(1), params(0))
    matchersNAB(queryPoolMatcher(notMatcherId), queryPoolMatcher(matcherId), checkerIds, flipOrder, id)
  }

  private def NABMatchers(matcher1:TMatcher, matcher2:TMatcher, checkerIds:Array[String], flipOrder:Boolean, id:Option[MId])
                         (implicit matchCheckerLib:SubMatchCheckerLib):TMatcher = {
    val (notMatcher, matcher) = if (!flipOrder) (matcher1, matcher2) else (matcher2, matcher1)
    matchersNAB(notMatcher, matcher, checkerIds, flipOrder, id)
  }

  sealed class MatcherTmplLib(val tmplDefs:List[MatcherTmplDef], val funcDefs:List[MatcherFuncDef]) {
    private val _templateMap = tmplDefs.map(td => (td.id, td)).toMap
    private val _funcMap = funcDefs.map(fd => (fd.id, fd)).toMap

    def spawn(tmplFuncId:String, _param:Array[Array[String]], regexDict:Map[String,String], id:Option[MId] = None, domainFinder:Option[DomainIdFinder] = None)
             (implicit matchCheckerLib:SubMatchCheckerLib):TMatcher = {
      if (_funcMap.contains(tmplFuncId)) {
        val funcDef = _funcMap(tmplFuncId)
        val funcMatchers = (0 until funcDef.templateCount).map{ idx =>
          val template = _templateMap(funcDef.templateId(idx))
          val param = funcDef.getParams(idx, _param)
          val domainParam = if (domainFinder.nonEmpty) param.map(getDomainIds(domainFinder.get, _)) else param
          val (t, checkerIds) = (template.t, template.getCheckerIds)
          if (funcDef.templateCount == 1) tmplSpawn(t, param, domainParam, regexDict, checkerIds, id, domainFinder)
          else tmplSpawn(t, param, domainParam, regexDict, checkerIds, None, domainFinder)
        }
        if (funcMatchers.size == 1) funcMatchers(0)
        else matchersOR(id, funcMatchers)
      }
      else {
        val templateDef = _templateMap(tmplFuncId)
        //val param = if (_funcMap.contains(tmplFuncId)) _funcMap.get(tmplFuncId).get.getParams(_param) else _param
        val domainParam = if (domainFinder.nonEmpty) _param.map(getDomainIds(domainFinder.get, _)) else _param
        val (t, checkerIds) = (templateDef.t, templateDef.getCheckerIds)
        tmplSpawn(t, _param, domainParam, regexDict, checkerIds, id, domainFinder)
      }
    }

    import org.dele.text.maen.AtomPropMatcherLib._
    private def tmplSpawn(tmplId:String, param:Array[Array[String]], domainParam:Array[Array[String]], regexDict:Map[String,String], checkerIds:Array[String], id:Option[MId] = None, domainFinder:Option[DomainIdFinder] = None)
             (implicit matchCheckerLib:SubMatchCheckerLib):TMatcher = {
      tmplId match {
        case "MTL_QueryFromPool" => {
          val queryParams = domainParam(0).toSet
          if (checkerIds.length < 1) queryPoolMatcher(queryParams)
          else {
            //if (checkerIds.length == 1) queryPoolMatcher(domainParam.toSet, checkerIds(0))
            //else matchersOR(id, checkerIds.map(queryPoolMatcher(domainParam.toSet, _)))
            queryPoolMatcher(queryParams, checkerIds)
          }
        }
        case "MTL_QueriesFromPool" => {
          if (domainParam.length != 2) throw Todo("MTL_QueriesFromPool only supports 2 params for now")
          val s1 = domainParam(0).toSet
          val s2 = domainParam(1).toSet
          queryAnd(s1, s2, checkerIds, id)
          //if (checkerIds.length < 1) queryAnd(s1, s2, NoCheckId, id)
          //else if (checkerIds.length == 1) queryAnd(s1, s2, checkerIds(0), id)
          //else matchersOR(id, checkerIds.map(queryAnd(s1, s2, _)))
        }
        case "MTL_RepetitionLongest" => {
          repeatMatcher(domainParam.map(dp => queryPoolMatcher(dp.toSet)), true, id, checkerIds)
        }
        case "MTL_RepetitionAll" => {
          repeatMatcher(domainParam.map(dp => queryPoolMatcher(dp.toSet)), false, id, checkerIds)
        }
        case "MTL_NotPreceededBy" => NABMatchers(tmplId, domainParam, checkerIds, false, id)
        case "MTL_NotFollowedBy" => NABMatchers(tmplId, domainParam, checkerIds, true, id)
        case "MTL_NotPreceededOrFollowedBy" => {
          if (domainParam.length != 3) throw MatcherTemplateErrorSpawnMatcher(tmplId, paramCountError(3))
          val nabParam = domainParam.slice(0, NABParamCount)
          val nab = NABMatchers("NaBNc.NaB", nabParam, checkerIds, false, None)
          NABMatchers(nab, queryPoolMatcher(domainParam(2).toSet), checkerIds, true, id)
        }
        case "MTL_NotOverlapWith" => {
          if (domainParam.length != 2) throw MatcherTemplateErrorSpawnMatcher(tmplId, paramCountError(2))
          val paramSets = domainParam.map(_.toSet)
          val matcher = queryPoolMatcher(paramSets(0))
          val notMatcher = queryPoolMatcher(paramSets(1))
          matchersNonOverlap(matcher, notMatcher, id)
        }
        case "MTL_PreceededBy" => {
          if (domainParam.length != 2) throw MatcherTemplateErrorSpawnMatcher(tmplId, paramCountError(2))
          val paramSets = domainParam.map(_.toSet)
          val expected = queryPoolMatcher(paramSets(0))
          val matcher = queryPoolMatcher(paramSets(1))
          matchersLookaround(expected, matcher, checkerIds, false, id)
        }
        case "MTL_FollowedBy" => {
          if (domainParam.length != 2) throw MatcherTemplateErrorSpawnMatcher(tmplId, paramCountError(2))
          val paramSets = domainParam.map(_.toSet)
          val expected = queryPoolMatcher(paramSets(1))
          val matcher = queryPoolMatcher(paramSets(0))
          matchersLookaround(expected, matcher, checkerIds, true, id)
        }
        case "MTL_HeadTailNotInbetween" => {
          if (domainParam.length < 3) throw MatcherTemplateErrorSpawnMatcher(tmplId, "At least 3 params required")
          val paramSets:Array[Set[String]] = domainParam.map(_.toSet)
          val ms = paramSets.map(ps => queryPoolMatcher(ps))
          if (domainParam.length == 3) matchersOrdered(ms, IndexedSeq(1), checkerIds, id)
          else {
            // merge neg matchers in between
            val negMatcher = matchersOR(ms.slice(1, ms.length-1))
            val matcherSeq = Seq(ms.head, negMatcher, ms.last)
            matchersOrdered(matcherSeq, IndexedSeq(1), checkerIds, id)
          }
        }
        case "MTL_Sequence" => {
          SeqMatchers(tmplId, domainParam, checkerIds, id)
        }

          //if (domainParam.length != 3) throw MatcherTemplateErrorSpawnMatcher(tmplFuncId, "exact 3 params required")
          //if (checkerIds.length == 1) matchersOrderedAllPositive(domainParam.map(queryPoolMatcher), matchCheckerLib, checkerIds(0), id)
          //else matchersOR(id, checkerIds.map(matchersOrderedAllPositive(domainParam.map(queryPoolMatcher), matchCheckerLib, _)))
        case "MTL_AtomSequence" => {
          //if (param.length != 1) throw MatcherTemplateErrorSpawnMatcher(tmplFuncId, paramCountError(1))
          val seqMatchers = param(0).map{str =>
            varLengthStringMatcher(str, checkerIds)
          } //param.map(p => { varLengthStringMatcher(p(0), checkerIds)
            //val words = p.split("\\s+")
            //if (checkerIds.length == 1) varLengthStringMatcher(p(0)) //matchersOrderedAllPositive(words.map(w => fromAtomMatcher(F(w))), matchCheckerLib, checkerIds(0))
            //else  //matchersOR(id, checkerIds.map(varLengthStringMatcher(_))) //matchersOrderedAllPositive(words.map(w => fromAtomMatcher(F(w))), matchCheckerLib, _)))
         // })
          matchersOR(id, seqMatchers)
        }
        case "MTL_AnyAtom" => {
          val count = param(0)(0).toInt
          anyAtomMatcher(count, checkerIds, id)
          //if (checkerIds.length == 1) anyAtomMatcher(count, checkerIds(0), id)
          //else matchersOR(id, checkerIds.map(anyAtomMatcher(count, _)))
        }
        case "MTL_AtomComposite" => {
          if (param.length < 3) throw MatcherTemplateErrorSpawnMatcher(tmplId, "At least 3 params required")
          fromAtomMatcher(AtomPropMatcherLib.spawn(TmplComposite, param, regexDict), EmptyCheckerIds, id)
          //if (checkerIds.length == 1) anyAtomMatcher(count, checkerIds(0), id)
          //else matchersOR(id, checkerIds.map(anyAtomMatcher(count, _)))
        }
        case x => throw NotImplemented(s"Unknown matcher template: $x")
      }
    }

    def contains(tmplFuncId:String):Boolean = _templateMap.contains(tmplFuncId) || _funcMap.contains(tmplFuncId)
  }
}