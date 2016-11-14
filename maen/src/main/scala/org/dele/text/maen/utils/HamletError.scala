package org.dele.text.maen.utils

/**
  * Created by jiaji on 2016-09-15.
  */

import java.util.regex.Pattern

import MaenError._

trait MaenError extends Throwable {
  val throwable:Throwable

  def describe:String = throwable.getMessage

  lazy val chain:List[MaenError] = {
    var tmp = throwable
    if (tmp.getCause != null) {
      val cause = new MaenErrorFromThrowable(tmp.getCause)
      cause :: cause.chain
    }
    else List(this)
  }
  lazy val stacks:Array[String] = {
    val c = Math.min(MaxStackTraceDepth, throwable.getStackTrace.length)
    val t = throwable.getStackTrace.take(c)
    t.map(stackTraceElement2String)
  }
  def describeAll = chain.map(_.describe)
  private def showStacks(regex:Pattern):String = {
    stacks.map{ st =>
      if (regex.matcher(st).matches) "\n\t" + st
      else "."
    }.mkString
  }

  def showStackChain(stackTraceFilter:String):String = {
    val regex = stackTraceFilter.r.pattern
    chain.map(_.showStacks(regex)).mkString("\n---Caused by:---\n")
  }
}

object MaenError {
  val NotImplemented = new MaenErrorBase("Not Implemented")

  def Todo(task:String) = new MaenErrorBase(task)

  val MaxStackTraceDepth = 20

  def stackTraceElement2String(e:StackTraceElement):String = s"${e.getClassName}::${e.getMethodName}  --  line ${e.getLineNumber}"

  class MaenErrorFromThrowable(val throwable:Throwable) extends MaenError {
  }

  implicit def Throwable2MaenError(th:Throwable) = new MaenErrorFromThrowable(th)

  class MaenErrorBase(msg:String) extends {
    val throwable = new Exception(msg)
  } with MaenError

  def handle(th:Throwable, filter:String = "org.dele.*"):String = {
    th.printStackTrace
    th match {
      case he:MaenError => {
        s"Maen Error: ${he.describe}\n%s".format(he.showStackChain(filter))
      }
      case t:Throwable => {
        val MaenError:MaenError = t
        s"Unknown Error: ${t.getMessage}\n%s".format(MaenError.showStackChain(filter))
      }
    }
  }
}