package org.dele.text.maen.utils

/**
  * Created by jiaji on 2016-09-15.
  */

import java.util.regex.Pattern

import HamletError._

trait HamletError extends Throwable {
  val throwable:Throwable

  def describe:String = throwable.getMessage

  lazy val chain:List[HamletError] = {
    var tmp = throwable
    if (tmp.getCause != null) {
      val cause = new HamletErrorFromThrowable(tmp.getCause)
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

object HamletError {
  val NotImplemented = new HamletErrorBase("Not Implemented")

  def Todo(task:String) = new HamletErrorBase(task)

  val MaxStackTraceDepth = 20

  def stackTraceElement2String(e:StackTraceElement):String = s"${e.getClassName}::${e.getMethodName}  --  line ${e.getLineNumber}"

  class HamletErrorFromThrowable(val throwable:Throwable) extends HamletError {
  }

  implicit def Throwable2HamletError(th:Throwable) = new HamletErrorFromThrowable(th)

  class HamletErrorBase(msg:String) extends {
    val throwable = new Exception(msg)
  } with HamletError

  def handle(th:Throwable, filter:String = "org.dele.*"):String = {
    th.printStackTrace
    th match {
      case he:HamletError => {
        s"Hamlet Error: ${he.describe}\n%s".format(he.showStackChain(filter))
      }
      case t:Throwable => {
        val hamletError:HamletError = t
        s"Unknown Error: ${t.getMessage}\n%s".format(hamletError.showStackChain(filter))
      }
    }
  }
}