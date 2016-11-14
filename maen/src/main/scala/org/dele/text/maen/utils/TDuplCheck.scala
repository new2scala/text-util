package org.dele.text.maen.utils

import org.dele.text.maen.AtomSeqMatch
import org.dele.text.maen.matchers.MatcherManager
import org.dele.text.maen.AtomSeqMatch
import org.dele.text.maen.extracts.Extract
import org.dele.text.maen.matchers.MatcherManager

import scala.collection.mutable.ListBuffer

/**
  * Created by jiaji on 2016-09-02.
  */
trait TDuplCheck[T] {
  val t:T
  def isDuplOf(ano:T):Boolean
}

object DuplCheck {

  def checkDupl[T](a:T, b:T)(implicit ev:T => TDuplCheck[T]):Boolean = a.isDuplOf(b)

  def checkCollDup[T](a:Iterable[T], b:Iterable[T])(implicit ev:T => TDuplCheck[T]):Boolean = a.forall(ai => b.exists(checkDupl(ai, _)))

  implicit class AtomSeqMatchDuplCheck(m:AtomSeqMatch) extends TDuplCheck[AtomSeqMatch] {
    val t = m
    def isDuplOf(ano:AtomSeqMatch):Boolean = {
      val t = MatcherManager.testCover(m, ano)
      // this covered by ano
      t.nonEmpty && t.get.eq(ano)
    }
  }

  implicit class RangeDuplCheck(range:Range) extends TDuplCheck[Range] {
    val t = range
    def isDuplOf(ano:Range):Boolean = range == ano
  }

  implicit class ExtractDuplCheck(e:Extract) extends TDuplCheck[Extract] {
    val t = e
    def isDuplOf(ano:Extract):Boolean = {
      if (e.name != ano.name) false
      else {
        if (!e.duplCheckable) true // ignore dupl checking (such as string-typed extracts
        else {
          //checkCollDup(e.instances, ano.instances)
          // instead of perform full check for all AtomSeqMatchs, we only check range of the AtomSeqMatchs
          val r1 = e.instances.map(_.range)
          val r2 = ano.instances.map(_.range)
          val rangeDup = checkCollDup(r1, r2)
          if (rangeDup) true
          else {
            val hasDiff1 = e.instances.exists(inst => Extract.notFoundIn(inst, ano.instances.toSet))
            val hasDiff2 = ano.instances.exists(inst => Extract.notFoundIn(inst, e.instances.toSet))
            !hasDiff1 && !hasDiff2
          }
        }
      }
    }
  }

  import scala.util.control.Breaks._
  def removeDupl[T](in:List[T])(implicit ev: T => TDuplCheck[T]):List[T] = {
    val result = ListBuffer[T]()
    in.foreach { i =>
      var isDup = false
      breakable {
        result.foreach { r =>
          if (checkDupl(i, r)) {
            isDup = true
            break
          }
          else if (checkDupl(r, i)) result -= r
        }
      }
      if (!isDup) result += i
    }

    result.toList
  }
}
