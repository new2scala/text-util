package org.dele.text.maen

import org.dele.text.maen.matchers.TMatcher.MId

/**
  * Created by jiaji on 2016-08-26.
  */
trait TContext {
  def getFlag(flag:String):Boolean
  def queryMatch(matcherId:MId):Boolean
  def getThreshold(key:String):Option[Double]
}

object ContextConstant {
  val EmptyContext = new TContext {
    override def queryMatch(matcherId: MId): Boolean = false
    override def getFlag(flag: String): Boolean = false
    override def getThreshold(key:String):Option[Double] = None
  }
}