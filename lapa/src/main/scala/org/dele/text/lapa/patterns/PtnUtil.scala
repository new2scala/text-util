package org.dele.text.lapa.patterns

/**
  * Created by jiaji on 2016-02-12.
  */

import TLangPattern.{PtnNode, PNId, LangPatternGroup}
import org.dele.text.maen.matchers.TMatcher

object PtnUtil {
  import org.dele.text.lapa.ErrorHandling._
  //class PtnApplication(val ptnId:String, val lang:String, val orderedNodesSet:IndexedSeq[List[PNId]], val lineTypeId:String)
  //def patternApplication(ptnId:String, lang:String, orderedNodesSet:IndexedSeq[List[PNId]], lineTypeId:String) = new PtnApplication(ptnId, lang, orderedNodesSet, lineTypeId)

  //private val EmptyNodesSet = IndexedSeq[List[PNId]]()
  //def patternApplication(ptnId:String, lang:String, lineTypeId:String) = new PtnApplication(ptnId, lang, EmptyNodesSet, lineTypeId)

  class PatternLib(private val _map:Map[String,TLangPattern]) {
    def get(ptnId:String):TLangPattern = if (_map.contains(ptnId)) _map.get(ptnId).get else throw new PatternLibErrorPtnNotFound(ptnId)
  }

  /*
  import TMatcher._

  import scala.collection.mutable
  def applyPtnGroup(ptnGroup:LangPatternGroup, ptnApps:AppliedPattern*):Set[TMatcher] = {
    val result = mutable.Set[TMatcher]()
    ptnApps.foreach(
      ptnApp => {
        val ptn = ptnGroup.patternById(ptnApp.ptnId)
        if (!ptn.isEmpty) {
          val appResult = applyPtn(ptn.get, ptnApp)
          result ++= appResult
          if (appResult.size > 1) result += matchersOR(ptnApp.ptnId, appResult)
        }
      }
    )
    val r = result.toSet
    //r + matchersOR(ptnGroup.id, r.toSeq)
    if (ptnGroup.patterns.size > 1) r + matchersOR(ptnGroup.id, r.toSeq) else r
  }
  */
}