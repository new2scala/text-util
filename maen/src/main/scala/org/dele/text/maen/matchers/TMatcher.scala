package org.dele.text.maen.matchers

import org.dele.text.maen.matchers.SubMatchCheckerLib._
import org.dele.text.maen.utils.HelperFuncs
import org.dele.text.maen._
import org.dele.text.maen.{AtomSeqMatch, TAtomMatcher, TInput, TMatchResultPool}

import scala.collection.mutable.ListBuffer

/**
  * Created by jiaji on 2016-02-09.
  */
import TMatcher._
trait TMatcher extends TMatcherDep {
  val id:Option[MId]
  def idEquals(id2:String) = if (id.isEmpty) false else id.get == id2
  def matchAt(resultPool:TMatchResultPool, index:Int):Set[AtomSeqMatch]
  def matchFrom(resultPool:TMatchResultPool,  start:Int):Set[AtomSeqMatch]
  def m(resultPool:TMatchResultPool) = matchFrom(resultPool, 0)
  protected def _matchFrom(resultPool:TMatchResultPool, start:Int):Set[AtomSeqMatch]
  val depth:Int
}

object TMatcher {
  import org.dele.text.maen.ErrorHandling._
  import org.dele.text.maen.AtomSeqMatch._
  import collection.mutable

  trait TMatcherDep {
    def depMatcherIds:Set[MId]
  }

  type MId = String
  val EmptyIdSet = Set[MId]()

  val EmptyCheckerIds = List[String]()
  abstract class MatcherBase(val id:Option[MId] = None,
                             val subMatchCheckerIds:Iterable[String] = EmptyCheckerIds)
                            (implicit val subMatchCheckerLib: SubMatchCheckerLib) extends TMatcher with MatchLimit {
    private def checkCache(resultPool:TMatchResultPool):Option[Set[AtomSeqMatch]] = resultPool.matcherOpCache.cached(this)

    protected final def getFromCacheOrCompute(resultPool:TMatchResultPool):Set[AtomSeqMatch] = {
      val cached:Option[Set[AtomSeqMatch]] = checkCache(resultPool)
      if (cached.isEmpty) {
        //println("[%s] cache hit(%d): %s".format(this.id, cache.get.size, cache.get.mkString("\t\t")))
        val results = _matchFrom(resultPool, 0)
        val limitAppliedResults = if (results.size <= MaxCount) results else {
          val ordered = results.toList.sortBy(m => m.range.start -> m.range.end)
          ordered.take(MaxCount).toSet
        }
        resultPool.matcherOpCache.cache(this, this.depMatcherIds, limitAppliedResults)
        limitAppliedResults
      }
      else cached.get
    }

    final def matchFrom(resultPool:TMatchResultPool, start:Int):Set[AtomSeqMatch] = {
      val c = getFromCacheOrCompute(resultPool)
      c.filter(_.range.start >= start)
    }

    final def matchAt(resultPool:TMatchResultPool, index:Int):Set[AtomSeqMatch] = {
      val matches = getFromCacheOrCompute(resultPool)
      matches.filter(_.range.start == index)
    }

    protected final def subMatchCheck(resultPool:TMatchResultPool, in:Set[AtomSeqMatch]):Set[AtomSeqMatch] = {
      if (subMatchCheckerIds.nonEmpty) {
        val submatchCheckers = subMatchCheckerIds.map(resultPool.getSubMatchChecker)
        in.filter(m => submatchCheckers.exists(_.check(m.subMatches, resultPool)))
      }
      else in
    }

    protected def _matchFrom(resultPool:TMatchResultPool, start:Int):Set[AtomSeqMatch]
    /*
    = {
      val uncheckedTmp = mutable.Set[AtomSeqMatch]() //(start to input.atoms.size-1).map(idx => matchAt(input, resultPool, idx))
      (start to resultPool.input.atoms.size-1).foreach{ idx =>
        uncheckedTmp ++= matchAt(resultPool, idx)
      }
      subMatchCheck(resultPool, uncheckedTmp.toSet)
    }
  */

    protected def filterBySubMatchCheckers(in:Set[AtomSeqMatch], resultPool: TMatchResultPool):Set[AtomSeqMatch] = {
      if (subMatchCheckerIds.isEmpty) in
      else {
        val submatchCheckers = subMatchCheckerIds.map(resultPool.getSubMatchChecker)
        in.filter(m => submatchCheckers.exists(_.check(m.subMatches, resultPool)))
      }
    }

    protected def filterBySubMatchCheckers(in:Iterable[Seq[AtomSeqMatch]], resultPool: TMatchResultPool):Iterable[Seq[AtomSeqMatch]] = {
      if (subMatchCheckerIds.isEmpty) in
      else {
        val submatchCheckers = subMatchCheckerIds.map(resultPool.getSubMatchChecker)
        in.filter(m => submatchCheckers.exists(_.check(m, resultPool)))
      }
    }

    def depMatcherIds:Set[MId] = {
      val depIdList = subMatchCheckerIds.map(subMatchCheckerLib.getDepMatcherIds)
      if (depIdList.nonEmpty) depIdList.reduce(_ ++ _)
      else EmptyIdSet
    }
    //def depMatcherIds:Set[MId] = EmptyIdSet
  }

  private type MatchAtFunc = (TMatchResultPool, Int) => Set[AtomSeqMatch]

  private trait TMatchAt {
    this: MatcherBase =>

    protected val matchAtFunc:MatchAtFunc
    override def _matchFrom(resultPool:TMatchResultPool, start:Int):Set[AtomSeqMatch] = {
      val uncheckedTmp = mutable.Set[AtomSeqMatch]() //(start to input.atoms.size-1).map(idx => matchAt(input, resultPool, idx))
      (start to resultPool.input.atoms.size-1).foreach{ idx =>
        uncheckedTmp ++= matchAtFunc(resultPool, idx)
      }
      subMatchCheck(resultPool, uncheckedTmp.toSet)
    }
  }

  /*
  private def _matchFromByIndices(resultPool:TMatchResultPool, start:Int, matchAtFunc:MatchAtFunc):Set[AtomSeqMatch] = {
    val uncheckedTmp = mutable.Set[AtomSeqMatch]() //(start to input.atoms.size-1).map(idx => matchAt(input, resultPool, idx))
    (start to resultPool.input.atoms.size-1).foreach{ idx =>
      uncheckedTmp ++= matchAtFunc(resultPool, idx)
    }
    uncheckedTmp.toSet
  }
  */

  //def _mergeMatchSetSeq(in:Seq[Set[AtomSeqMatch]]):Set[AtomSeqMatch] = in.foldLeft(Set[AtomSeqMatch]())(_ ++ _)

  trait AtomMatchLimit extends MatchLimit {
    override val MaxCount = MatcherManager.MaxMatchCount*2
  }

  private[TMatcher] class _FromAtomMatcher(val atomMatcher:TAtomMatcher,
                                           subMatchCheckerIds:Iterable[String] = EmptyCheckerIds,
                                           id:Option[MId] = None)
                                          (implicit subMatchCheckerLib: SubMatchCheckerLib) extends MatcherBase(id, subMatchCheckerIds) with AtomMatchLimit with TMatchAt {
    override def toString = "[%s:%s]".format(
      if (id.isEmpty) "(NoId)" else id.get,
      atomMatcher
    )

    protected val matchAtFunc:MatchAtFunc = (resultPool, index) => {
      if (atomMatcher.check(resultPool.input.atoms(index))) Set(from(resultPool, index, this))
      else EmptyMatchResult
    }

    val depth = 1
  }
  def fromAtomMatcher(atomMatcher:TAtomMatcher,
                      subMatchCheckerIds:Iterable[String] = EmptyCheckerIds,
                      id:Option[MId] = None)
                     (implicit subMatchCheckerLib: SubMatchCheckerLib):TMatcher = new _FromAtomMatcher(atomMatcher, subMatchCheckerIds, id)

  import scala.util.control.Breaks._
  private[TMatcher] class _VarLengthStringMatcher(val string2Match:String,
                                                  caseSensitive:Boolean,
                                                  subMatchCheckerIds:Iterable[String] = EmptyCheckerIds,
                                                  id:Option[MId] = None)
                                                 (implicit subMatchCheckerLib: SubMatchCheckerLib) extends MatcherBase(id, subMatchCheckerIds) with TMatchAt {
    private val matchStr = if (caseSensitive) string2Match else string2Match.toLowerCase
    protected val matchAtFunc:MatchAtFunc = (resultPool, index) => {
      var idx = index
      var lastAtomIndex = -1
      var remStr = matchStr
      val input = resultPool.input
      breakable {
        while (idx < input.atoms.size) {
          var txt = input.atoms(idx).text
          if (!caseSensitive) txt = txt.toLowerCase
          if (!remStr.startsWith(txt)) break
          remStr = remStr.substring(txt.length).trim
          if (remStr.isEmpty) {
            lastAtomIndex = idx
            break
          }
          idx = idx + 1
        }
      }
      if (lastAtomIndex >= 0) Set(from(resultPool, index to lastAtomIndex, this))
      else Set()
    }

    val depth = 1
  }

  def varLengthStringMatcher(string2Match:String,
                             subMatchCheckerIds:Iterable[String] = EmptyCheckerIds,
                             caseSensitive:Boolean = false,
                             id:Option[MId] = None)
                            (implicit subMatchCheckerLib: SubMatchCheckerLib) = new _VarLengthStringMatcher(string2Match, caseSensitive, subMatchCheckerIds, id)

  private[TMatcher] class _AnyAtomMatcher(val count:Int,
                                          subMatchCheckerIds:Iterable[String] = EmptyCheckerIds,
                                          id:Option[MId] = None)
                                         (implicit subMatchCheckerLib: SubMatchCheckerLib) extends MatcherBase(id, subMatchCheckerIds) with AtomMatchLimit with TMatchAt {
    protected val matchAtFunc:MatchAtFunc = (resultPool, index) => {
      val endIndex = index + count - 1
      if (endIndex < resultPool.input.atoms.length) Set(from(resultPool, index to endIndex, this))
      else Set()
    }

    val depth = 1
  }

  def anyAtomMatcher(count:Int,
                     subMatchCheckerIds:Iterable[String] = EmptyCheckerIds,
                     id:Option[MId] = None)
                    (implicit subMatchCheckerLib: SubMatchCheckerLib):TMatcher = new _AnyAtomMatcher(count, subMatchCheckerIds, id)

  private[TMatcher] abstract class _CompositeMatcherBase(val subMatchers:Seq[TMatcher],
                                                         subMatchCheckerIds:Iterable[String] = EmptyCheckerIds,
                                                         id:Option[MId])
                                                        (implicit subMatchCheckerLib: SubMatchCheckerLib) extends MatcherBase(id, subMatchCheckerIds) {
    //todo: query from Match result pool instead
    def subMatchAt(subMatcher:TMatcher, resultPool:TMatchResultPool, index:Int):Set[AtomSeqMatch] =
      if (subMatcher.id.isEmpty) subMatcher.matchAt(resultPool, index) else resultPool.query(subMatcher.id.get)
    override def depMatcherIds:Set[MId] = {
      //super.depMatcherIds ++ subMatchers.flatMap(_.id).toSet ++ subMatchers.flatMap(_.depMatcherIds)
      val p1 = super.depMatcherIds
      val p2 = subMatchers.flatMap{ sm =>
        if (sm.id.nonEmpty) Set(sm.id.get)
        else sm.depMatcherIds
      }
      p1 ++ p2
    }
    //def matchAt(input:TInput, resultPool:TMatchResultPool, index:Int):Set[AtomSeqMatch] = throw NotImplemented
    val depth = subMatchers.maxBy(_.depth).depth + 1
  }

  private[TMatcher] class _MatchersOR(subMatchers:Seq[TMatcher],
                                      subMatchCheckerIds:Iterable[String] = EmptyCheckerIds,
                                      id:Option[MId])
                                     (implicit subMatchCheckerLib: SubMatchCheckerLib) extends _CompositeMatcherBase(subMatchers, subMatchCheckerIds, id) with TMatchAt {
    private val subMatcherSet = subMatchers.toSet
    protected val matchAtFunc:MatchAtFunc = (resultPool, index) => {
      val alts:Set[AtomSeqMatch] = subMatcherSet.flatMap((m:TMatcher) => subMatchAt(m, resultPool, index))
      alts.map(a => from(resultPool, this, List(a)))
    }
    override def toString = subMatchers.map(sm => "(%s)".format(sm.toString)).mkString(" OR ")
    //override def depMatcherIds:Set[MId] = subMatchers.flatMap(_.id).toSet ++ subMatchers.map(_.depMatcherIds).reduce(_ ++ _)
  }
  def matchersOR(id:String,
                 subMatchers:Seq[TMatcher])
                (implicit subMatchCheckerLib: SubMatchCheckerLib) = new _MatchersOR(subMatchers, EmptyCheckerIds, Option(id))
  def matchersOR(id:Option[String],
                 subMatchers:Seq[TMatcher])
                (implicit subMatchCheckerLib: SubMatchCheckerLib) = new _MatchersOR(subMatchers, EmptyCheckerIds, id)
  def matchersOR(subMatchers:Seq[TMatcher])
                (implicit subMatchCheckerLib: SubMatchCheckerLib) = new _MatchersOR(subMatchers, EmptyCheckerIds, None)

  private val EmptyResults:Set[List[AtomSeqMatch]] = Set()
  import org.dele.text.maen.utils.HelperFuncs._

  private def allMatches(resultPool:TMatchResultPool, matchers:Seq[TMatcher]):Seq[Set[AtomSeqMatch]] = matchers.map(_.m(resultPool))

  //private type _MatchCheckFunc = (TMatchResultPool, Int) => Set[AtomSeqMatch]
  private def orderedMatchesFrom(input:TInput, resultPool:TMatchResultPool, matchers:Seq[TMatcher], start:Int, prev:List[AtomSeqMatch], matchesAllAfter:Boolean):Set[List[AtomSeqMatch]] = {
    if (matchers.isEmpty) Set(prev)
    else {
      if (start >= input.atoms.size) EmptyResults
      else {
        val (head, tail) = (matchers.head, matchers.tail)
        val headMatches = if (matchesAllAfter) head.matchFrom(resultPool, start) else head.matchAt(resultPool, start)
        if (headMatches.isEmpty) EmptyResults
        else {
          val resultList = mutable.Set[Set[List[AtomSeqMatch]]]()
          headMatches.foreach { hm =>
            val curr = prev :+ hm
            val currResult = orderedMatchesFrom(input, resultPool, tail, hm.range.end + 1, curr, matchesAllAfter)
            if (currResult.nonEmpty) resultList += currResult
          }
          resultList.toSet.flatten
        }
      }
    }
  }

  private def orderedMatchesFromLNG(input:TInput, resultPool:TMatchResultPool, matchers:Seq[TMatcher], start:Int, prev:List[AtomSeqMatch]):Set[List[AtomSeqMatch]] = {
    val all = (start until input.atoms.size).map{ idx => orderedMatchesFrom(resultPool.input, resultPool, matchers, idx, List(), false) }
    if (all.nonEmpty) all.flatten.toSet
    else Set[List[AtomSeqMatch]]()
    //orderedMatchesFrom(input, resultPool, matchers, start, prev, false)
  }

  private def checkNAB(notMatches:Set[AtomSeqMatch], matches:Set[AtomSeqMatch], subMatchCheckerIds:Iterable[String], resultPool:TMatchResultPool, flipOrder:Boolean):Set[AtomSeqMatch] = {
    if (matches.isEmpty) EmptyMatchResult
    else {
      if (notMatches.isEmpty) matches
      else {
        // no check needed
        val p1 = if (!flipOrder) {
          matches.filter(m => notMatches.forall(_.range.end >= m.range.start))
        }
        else {
          matches.filter(m => notMatches.forall(_.range.start <= m.range.end))
        }
        // ordered
        val (matches1, matches2) = if (!flipOrder) (notMatches, matches) else (matches, notMatches)
        val tocheck = for (m1 <- matches1; m2 <- matches2 if m1.range.end < m2.range.start) yield Seq(m1, m2)
        if (tocheck.isEmpty) p1
        else {
          val p2 = if (subMatchCheckerIds.isEmpty) EmptyMatchResult
          else {
            val submatchCheckers = subMatchCheckerIds.map(resultPool.getSubMatchChecker)
            val checked = tocheck.filter(x => submatchCheckers.forall(!_.check(x, resultPool)))
            //val checked = subMatchChecker(tocheck, subMatchCheckerIds, resultPool)
            if (!flipOrder) checked.map(_.last) else checked.map(_.head)
          }
          p1 ++ p2
        }
      }
    }
  }

  private def checkAB(ms1:Set[AtomSeqMatch], ms2:Set[AtomSeqMatch], subMatchCheckerIds:Iterable[String], resultPool:TMatchResultPool, flipOrder:Boolean):Set[AtomSeqMatch] = {
    if (ms1.isEmpty || ms2.isEmpty) EmptyMatchResult
    else {
      // ordered
      val (matches1, matches2) = if (!flipOrder) (ms1, ms2) else (ms2, ms1)
      val tocheck = for (m1 <- matches1; m2 <- matches2 if m1.range.end < m2.range.start) yield Seq(m1, m2)
      if (tocheck.isEmpty) EmptyMatchResult
      else {
        val p2 = if (subMatchCheckerIds.isEmpty) EmptyMatchResult
        else {
          val submatchCheckers = subMatchCheckerIds.map(resultPool.getSubMatchChecker)
          val checked = tocheck.filter(x => submatchCheckers.exists(_.check(x, resultPool)))
          //val checked = subMatchChecker(tocheck, subMatchCheckerIds, resultPool)
          if (!flipOrder) checked.map(_.last) else checked.map(_.head)
        }
        p2
      }
    }
  }

  /// matcher used for specific purpose: (A) B .or. A (B)
  private[TMatcher] class _MatchersLookaroundAB(
                                               private val expected:TMatcher,
                                               private val matcher:TMatcher,
                                               subMatchCheckerIds:Iterable[String],
                                               val flipOrder:Boolean, // '(expected) matcher' flipped:  matcher (expected)
                                               id:Option[MId] = None
                                             )(implicit subMatchCheckerLib: SubMatchCheckerLib) extends _CompositeMatcherBase(Seq(expected, matcher), subMatchCheckerIds, id) with TMatchAt {

    /*
    override def matchFrom(input:TInput, resultPool:TMatchResultPool, start:Int):Set[AtomSeqMatch] = {
      //val uncheckedMatches = matchFromUnchecked(input, resultPool, start)

      val matches1 = matcher1.matchFrom(input, resultPool, start)
      val matches2 = matcher2.matchFrom(input, resultPool, start)

      check_NAB(matches1, matches2, subMatchCheckerIds, resultPool, flipOrder)
    }
    */

    protected val matchAtFunc:MatchAtFunc = (resultPool, index) => {
      //todo: optimize
      val matches = matcher.matchAt(resultPool, index)
      if (matches.isEmpty) EmptyMatchResult
      else {
        val expMatches = expected.m(resultPool)
        checkAB(expMatches, matches, subMatchCheckerIds, resultPool, flipOrder)
      }
    }

    override def toString = if (!flipOrder) s"expected($expected) $matcher" else s"$matcher expected($expected) "
  }
  def matchersLookaround(expected:TMatcher, matcher:TMatcher, subMatchCheckerIds:Iterable[String] = EmptyCheckerIds, flipOrder:Boolean = false, id:Option[MId] = None)(implicit subMatchCheckerLib: SubMatchCheckerLib) =
    new _MatchersLookaroundAB(expected, matcher, subMatchCheckerIds, flipOrder, id)

  /// matcher used for specific purpose: Not(A) B .or. A Not(B)
  private[TMatcher] class _MatchersOrderedNAB(
                                               private val notMatcher:TMatcher,
                                               private val matcher:TMatcher,
                                               subMatchCheckerIds:Iterable[String],
                                               val flipOrder:Boolean,
                                               id:Option[MId] = None
                                             )(implicit subMatchCheckerLib: SubMatchCheckerLib) extends _CompositeMatcherBase(Seq(notMatcher, matcher), subMatchCheckerIds, id) with TMatchAt {

    /*
    override def matchFrom(input:TInput, resultPool:TMatchResultPool, start:Int):Set[AtomSeqMatch] = {
      //val uncheckedMatches = matchFromUnchecked(input, resultPool, start)

      val matches1 = matcher1.matchFrom(input, resultPool, start)
      val matches2 = matcher2.matchFrom(input, resultPool, start)

      check_NAB(matches1, matches2, subMatchCheckerIds, resultPool, flipOrder)
    }
    */

    protected val matchAtFunc:MatchAtFunc = (resultPool, index) => {
      //todo: optimize
      val matches = matcher.matchAt(resultPool, index)
      if (matches.isEmpty) EmptyMatchResult
      else {
        val notMatches = notMatcher.m(resultPool)
        checkNAB(notMatches, matches, subMatchCheckerIds, resultPool, flipOrder)
      }
    }

    override def toString = if (!flipOrder) s"Not($notMatcher) $matcher" else s"$matcher Not($notMatcher) "
  }

  private[TMatcher] class _MatchersAB_NotOverlap(
                                               private val matcher:TMatcher,
                                               private val notMatcher:TMatcher,
                                               id:Option[MId] = None
                                             )(implicit subMatchCheckerLib: SubMatchCheckerLib) extends _CompositeMatcherBase(Seq(matcher, notMatcher), EmptyCheckerIds, id) with TMatchAt {

    /*
    override def matchFrom(input:TInput, resultPool:TMatchResultPool, start:Int):Set[AtomSeqMatch] = {
      //val uncheckedMatches = matchFromUnchecked(input, resultPool, start)

      val matches1 = matcher1.matchFrom(input, resultPool, start)
      val matches2 = matcher2.matchFrom(input, resultPool, start)

      check_NAB(matches1, matches2, subMatchCheckerIds, resultPool, flipOrder)
    }
    */

    protected val matchAtFunc:MatchAtFunc = (resultPool, index) => {
      //todo: optimize
      val matches = matcher.matchAt(resultPool, index)
      if (matches.isEmpty) EmptyMatchResult
      else {
        val notMatches = notMatcher.m(resultPool)
        matches.filter(m => notMatches.forall(!_.isOverlap(m)))
      }
    }

    override def toString = s"($matcher) NotOverlap with ($notMatcher)"
  }

  def matchersNonOverlap(matcher:TMatcher, notMatcher:TMatcher, id:Option[MId] = None)(implicit subMatchCheckerLib: SubMatchCheckerLib) =
    new _MatchersAB_NotOverlap(matcher, notMatcher, id)

  def matchersNAB(notMatcher:TMatcher, matcher:TMatcher, subMatchCheckerIds:Iterable[String] = EmptyCheckerIds, flipOrder:Boolean = false, id:Option[MId] = None)
                 (implicit subMatchCheckerLib: SubMatchCheckerLib) =
    new _MatchersOrderedNAB(notMatcher, matcher, subMatchCheckerIds, flipOrder, id)

  //val profCount = mutable.Map[String, Int]()

  private[TMatcher] class _MatchersALLOrdered(
     subMatchers:Seq[TMatcher],
     negMatcherIndices:IndexedSeq[Int],
     subMatchCheckerIds:Iterable[String] = EmptyCheckerIds,
     id:Option[MId]
  )(implicit subMatchCheckerLib: SubMatchCheckerLib)
    extends _CompositeMatcherBase(subMatchers, subMatchCheckerIds, id) {
    private val _posMatchers:Seq[TMatcher] = subMatchers.indices.filter(!negMatcherIndices.contains(_)).map(subMatchers)


    import scala.collection.mutable
    private val _negMatchers:Map[Int, TMatcher] = {
      val transIndices = negMatcherIndexTransform(negMatcherIndices)
      (transIndices zip negMatcherIndices.map(subMatchers)).toMap
      //negMatcherIndices.map(subMatchers)
    }


    import scala.util.control.Breaks._
    private def checkNegMatchers(subMatches:Seq[AtomSeqMatch], resultPool: TMatchResultPool):Boolean = {
      if (_negMatchers.isEmpty) true
      else {
        var ranges = allGaps(resultPool.input.atoms.indices, subMatches.map(_.range))
        var foundNeg = false
        breakable {
          _negMatchers.foreach { p =>
            val idx = p._1
            val matcher = p._2
            if (ranges(idx).nonEmpty) {
              val r = ranges(idx).get
              val matches = matcher.m(resultPool) //todo: room for optimization
              val f = matches.exists(_.range.intersect(r).nonEmpty)
              if (f) {
                foundNeg = true
                break
              }
            }
          }
        }
        !foundNeg
      }
    }

    private def isLNG = subMatchCheckerIds.size == 1 && subMatchCheckerIds.head == ListNGramId

    override protected def _matchFrom(resultPool:TMatchResultPool, start:Int):Set[AtomSeqMatch] = {
      val checkedMatches:Set[List[AtomSeqMatch]] = if (isLNG) orderedMatchesFromLNG(resultPool.input, resultPool, _posMatchers, start, List())
      else {
        val uncheckedMatches = orderedMatchesFrom(resultPool.input, resultPool, _posMatchers, start, List(), true) //matchFromUnchecked(resultPool.input, resultPool, start)
        val checkedNeg = uncheckedMatches.filter(checkNegMatchers(_, resultPool))
        if (subMatchCheckerIds.nonEmpty) {
          val submatchCheckers = subMatchCheckerIds.map(resultPool.getSubMatchChecker)
          checkedNeg.filter(x => submatchCheckers.exists(_.check(x, resultPool)))
        }
        else checkedNeg
      }
      checkedMatches.map(from(resultPool, this, _))
    }

    override def toString = subMatchers.map(sm => "(%s)".format(sm.toString)).mkString(" ")

  }

  import SubMatchCheckerLib._

  private val EmptyNegMatcherIndexes = IndexedSeq[Int]()
  def matchersOrderedAllPositive(subMatchers:Seq[TMatcher], subMatchCheckerIds:Iterable[String] = EmptyCheckerIds, id:Option[MId] = None)
                                (implicit subMatchCheckerLib: SubMatchCheckerLib) =
    new _MatchersALLOrdered(subMatchers, EmptyNegMatcherIndexes, subMatchCheckerIds, id)
  //def matchersOrderedAllPositive(subMatchers:Seq[TMatcher], subMatchCheckerLib: SubMatchCheckerLib, subMatchCheckerId:String = NoCheckId) =
  //  new _MatchersALLOrdered(subMatchers, EmptyNegMatcherIndexes, subMatchCheckerLib, subMatchCheckerId, None)

  def matchersOrdered(subMatchers:Seq[TMatcher], negMatcherIndexes:IndexedSeq[Int], subMatchCheckerIds:Iterable[String] = EmptyCheckerIds, id:Option[MId] = None)
                     (implicit subMatchCheckerLib: SubMatchCheckerLib) =
    new _MatchersALLOrdered(subMatchers, negMatcherIndexes, subMatchCheckerIds, id)


  private[TMatcher] class _QueryFromResultPool(val resultIds:Set[String],
                                               subMatchCheckerIds:Iterable[String] = EmptyCheckerIds,
                                               id:Option[MId] = None)
                                              (implicit subMatchCheckerLib: SubMatchCheckerLib) extends MatcherBase(id, subMatchCheckerIds) with TMatchAt {
    protected val matchAtFunc:MatchAtFunc = (resultPool, index) => {
      val allMatches = m(resultPool)
      allMatches.filter(_.range.start == index)
    }
    override def m(resultPool:TMatchResultPool) = resultIds.flatMap(resultPool.query)
    override def toString:String = "ResultPool Query: [%s]".format(resultIds.mkString(","))
    override def depMatcherIds = super.depMatcherIds ++ resultIds
    val depth = 1
  }

  def queryPoolMatcher(resultIds:Set[String],
                       subMatchCheckerIds:Iterable[String] = EmptyCheckerIds,
                       id:Option[MId] = None)
                      (implicit subMatchCheckerLib: SubMatchCheckerLib) = new _QueryFromResultPool(resultIds, subMatchCheckerIds, id)
  def queryPoolMatcher(resultId:String)
                      (implicit subMatchCheckerLib: SubMatchCheckerLib) = new _QueryFromResultPool(Set(resultId), EmptyCheckerIds, None)

  private[TMatcher] class _QueryAND(val resultIds1:Set[String],
                                    val resultIds2:Set[String],
                                    subMatchCheckerIds:Iterable[String] = EmptyCheckerIds,
                                    id:Option[MId] = None)
                                   (implicit subMatchCheckerLib: SubMatchCheckerLib) extends MatcherBase(id, subMatchCheckerIds) with TMatchAt {
    private val _q1 = queryPoolMatcher(resultIds1, subMatchCheckerIds)
    private val _q2 = queryPoolMatcher(resultIds2, subMatchCheckerIds)
    protected val matchAtFunc:MatchAtFunc = (resultPool, index) => {
      val allMatches = m(resultPool)
      allMatches.filter(_.range.start == index)
    }

    override def m(resultPool:TMatchResultPool) = {
      val all = for (m1 <- _q1.m(resultPool); m2 <- _q2.m(resultPool)) yield from(resultPool, this, List(m1, m2))
      all.take(MatcherManager.MaxMatchCount) // break when there are too many matches
    }
    override def toString:String = "ResultPool Query: [%s] AND [%s]".format(resultIds1.mkString(","), resultIds2.mkString(","))
    override def depMatcherIds = super.depMatcherIds ++ resultIds1 ++ resultIds2
    val depth = 1
  }

  def queryAnd(resultIds1:Set[String],
               resultIds2:Set[String],
               subMatchCheckerIds:Iterable[String] = EmptyCheckerIds,
               id:Option[MId] = None)
              (implicit subMatchCheckerLib: SubMatchCheckerLib) = new _QueryAND(resultIds1, resultIds2, subMatchCheckerIds, id)


  private val EmptyMatchResult = Set[AtomSeqMatch]()

  private def rangeContain(container:Range, containee:Range):Boolean = container.start <= containee.start && container.end >= containee.end

  /// bad idea: cause dead-loops,
  /// update: used only for Longest repetition matcher
  private def mergeByRange(in:Set[AtomSeqMatch]):Set[AtomSeqMatch] = {
    val r = mutable.Set[AtomSeqMatch]()
    in.foreach{ m =>
      if (!r.exists(x => rangeContain(x.range, m.range))) {
        val toRemove = r.filter(x => rangeContain(m.range, x.range))
        r --= toRemove
        r += m
      }
    }
    r.toSet
  }

  trait MatchLimit {
    val MaxCount = MatcherManager.MaxMatchCount
    def countCheck(count:Int):Boolean = count <= MaxCount
  }

  private[TMatcher] class _RepeatableMatchers(subMatchers:Seq[TMatcher], onlyKeepLongest:Boolean, id:Option[MId], subMatchCheckerIds:Iterable[String] = EmptyCheckerIds)
                                             (implicit subMatchCheckerLib: SubMatchCheckerLib)
    extends _CompositeMatcherBase(subMatchers, subMatchCheckerIds, id) {
    private val _orMatcher = matchersOR(id + "._int_or_", subMatchers)

    private def connectNext(curr:AtomSeqMatch, following:List[AtomSeqMatch], resultPool:TMatchResultPool, subMatchCheckers:Iterable[TSubMatchChecker]):Set[AtomSeqMatch] = {
      if (following.isEmpty) EmptyMatchResult
      else {
        val r = mutable.Set[AtomSeqMatch]()
        breakable {
          following.foreach{ fm =>
            val seq = List(curr, fm)
            if (subMatchCheckers.exists(_.check(seq, resultPool))) {
              r += from(resultPool, this, seq)
              if (!countCheck(r.size)) break
            }
            else break
          }
        }
        r.toSet
      }
    }

    private def connectAll(allMatches:Set[AtomSeqMatch], resultPool:TMatchResultPool, subMatchCheckers:Iterable[TSubMatchChecker]):Set[AtomSeqMatch] = {
      val r = mutable.Set[AtomSeqMatch]()
      var toCheck = ListBuffer[AtomSeqMatch]()
      val matches2Check = if (onlyKeepLongest) mergeByRange(allMatches) else allMatches
      toCheck ++= matches2Check.toList.sortBy(a => a.range.start -> a.range.end)
      breakable{
        while (toCheck.nonEmpty && countCheck(r.size)) {
          val c = toCheck.head
          val ct = toCheck.tail.filter(_.range.start > c.range.end).toList
          val cn = connectNext(c, ct, resultPool, subMatchCheckers)
          if (onlyKeepLongest) {
            if (cn.isEmpty) r += c
            val nextIdx = if (cn.isEmpty) c.range.end else cn.maxBy(_.range.end).range.end
            toCheck = toCheck.filter(_.range.start > nextIdx)
          }
          else { // keeps all the combinations
            r += toCheck.remove(0)
          }
          cn.foreach(x => toCheck.insert(0, x))
        }
      }
      r.toSet
    }

    override protected def _matchFrom(resultPool:TMatchResultPool, start:Int):Set[AtomSeqMatch] = {
      val allMatches = _orMatcher.matchFrom(resultPool, start)
      if (allMatches.nonEmpty) {
        if (subMatchCheckerIds.isEmpty) throw NotImplemented("_RepeatableMatchers: matchers without submatch checkers not implemented (allowed)")
        val subMatchCheckers = subMatchCheckerIds.map(resultPool.getSubMatchChecker)
        connectAll(allMatches, resultPool, subMatchCheckers)
      }
      else EmptyMatchResult
    }

/*
    override def matchAt(resultPool:TMatchResultPool, index:Int):Set[AtomSeqMatch] = {
      val allMatches = _orMatcher.matchAt(resultPool, index)
      if (allMatches.isEmpty) EmptyMatchResult
      else {
        val matchesFrom = matchFrom(resultPool, index)
        matchesFrom.filter(_.range.start == index) // todo: optimize, no need to compute all matches from 'index'
      }
    }
    */
  }

  def repeatMatcher(subMatchers:Seq[TMatcher], onlyKeepLongest:Boolean, id:Option[MId], subMatchCheckerIds:Iterable[String])
                   (implicit subMatchCheckerLib: SubMatchCheckerLib) = new _RepeatableMatchers(subMatchers, onlyKeepLongest, id, subMatchCheckerIds)

  //def ListNGram(atomMatchers:List[TMatcher], subMatchCheckerLib: SubMatchCheckerLib):TMatcher = matchersOrderedAllPositive(atomMatchers, subMatchCheckerLib, ListNGramId)
  def ListNGram(atomMatchers:List[TMatcher], id:Option[MId] = None)
               (implicit subMatchCheckerLib: SubMatchCheckerLib):TMatcher = matchersOrderedAllPositive(atomMatchers, List(ListNGramId), id)
}