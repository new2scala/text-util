package org.dele.text.maen.test

import org.dele.text.maen.{AtomSeqMatch, TMatchResultPool}
import org.dele.text.maen.matchers.MatcherManager
import org.dele.text.maen.matchers.TMatcher.MId
import org.dele.text.maen.matchers.MatcherManager
import org.dele.text.maen.{AtomSeqMatch, TMatchResultPool}
import org.joda.time.DateTime

/**
  * Created by jiaji on 2016-09-17.
  */
object TestProfiler {
  class Counter private[TestProfiler] {
    private var total = 0
    //def incTotal:Unit = total = total+1
    //def totalSum:Int = total
    private val start = DateTime.now.getMillis

    import collection.mutable
    private var matcherCounters = mutable.Map[MId, Int]()
    def incMCount(mid:MId):Int = {
      if (!matcherCounters.contains(mid)) matcherCounters(mid) = 0
      matcherCounters(mid) = matcherCounters(mid) + 1
      total = total + 1
      matcherCounters(mid)
    }

    def showStatsByName:Unit = {
      val ordered = matcherCounters.toList.sortBy(_._1)
      println(ordered.mkString("\n"))
      totolRuns
    }

    def totolRuns = {
      println("----------------------")
      println(s"         total: $total")
    }
    def showStatsByCount(mm:MatcherManager):Unit = {
      val ordered = matcherCounters.toList.sortBy(_._2).reverse
      println(ordered.map{ p =>
        val matcher = mm.getMatcherBy(p._1)
        "%s (depth:%d): %d\n\t%s".format(p._1, matcher.depth, p._2, matcher.depMatcherIds.mkString("\n\t"))
      }.mkString("\n"))
      totolRuns
    }

    def showMatchStatsByDepth(mp:TMatchResultPool):Unit = {
      val ids = mp.allMatcherIds
      val allMatches = ids.map(mp.query(_)).filter(_.nonEmpty).map(_.maxBy(_.depth))
      val orderedByDepth = allMatches.toList.sortBy(_.depth)
      println("----------------------")
      println(orderedByDepth.map{ m =>
        "[%s] (depth:%d)".format(m, m.depth)
      }.mkString("\n"))
      println("----------------------")
      println("Deepest match:")
      followDeepestPath(orderedByDepth.last)
    }

    private def followDeepestPath(m:AtomSeqMatch):Unit = {
      val curr = m.subMatches.mkString("\t")
      println(curr)
      if (m.subMatches.nonEmpty) {
        val deepest = m.subMatches.maxBy(_.depth)
        followDeepestPath(deepest)
      }
    }


    def showTimeElapsed = println("%d ms".format(DateTime.now.getMillis - start))
  }

  def newCounter:Counter = new Counter
}
