package org.dele.text.maen

import org.dele.text.maen.TAtomMatcher.PropMatchBase
import org.dele.text.maen.utils.HamletError

import scala.util.{Failure, Success, Try}

/**
  * Created by jiaji on 2016-02-09.
  */
trait TPropMatcherTmpl {
  val id:String
  private[maen] def _expectedParamCount:Int
  def checkParamCount(params:Array[Array[String]]):Boolean = params.length == _expectedParamCount
  //val matchType:PropMatchType,
  //val caseSensi:Boolean,
  val exclude:Boolean
  def spawn(params:Array[Array[String]], regexDict:Map[String,String]):Try[TAtomMatcher]
}

object TPropMatcherTmpl {
  object PropMatchType extends Enumeration {
    type PropMatchType = Value
    val Equals, AtLeastOne, All, Regex = Value
  }

  import PropMatchType._
  import TAtomMatcher._
  import ErrorHandling._

  private def try2GetParamOf[T](c:Class[T], p:AnyRef):Try[T] = {
    if (c.isAssignableFrom(p.getClass)) Success(p.asInstanceOf[T])
    else Failure(AtomPropMatcherLibErrorParamType(c.toString, p.getClass.toString))
    /*
    p match {
      case p:T => Success(p)
      case x =>
    }
    */
  }
  private def try2GetString(p:AnyRef):Try[String] = try2GetParamOf[String](classOf[String], p)
  private def try2GetStringArray(p:AnyRef):Try[Array[String]] = try2GetParamOf[Array[String]](classOf[Array[String]], p)


  private val KnownPropMatcherParamCount = 1
  private[TPropMatcherTmpl] class _KnownPropMatcher(val propName:String, val id:String, val matchType:PropMatchType, val caseSensi:Boolean, val exclude:Boolean)
    extends TPropMatcherTmpl {
    private[maen] def _expectedParamCount:Int = KnownPropMatcherParamCount
    override def spawn(params:Array[Array[String]], regexDict:Map[String,String]):Try[TAtomMatcher] = {
      if (!checkParamCount(params)) Failure(AtomPropMatcherLibErrorParamCount(KnownPropMatcherParamCount, params.length))
      else {
        val m = if (propName == texts) textMatcher(params(0), caseSensi) else propMatchExact(propName, params(0), caseSensi, exclude)
        Success(m)
      }
    }
  }

  private[TPropMatcherTmpl] class _KnownPropRegexMatcher(val propName:String, val id:String, val exclude:Boolean)
    extends TPropMatcherTmpl {
    private[maen] def _expectedParamCount:Int = KnownPropMatcherParamCount
    override def spawn(params:Array[Array[String]], regexDict:Map[String,String]):Try[TAtomMatcher] = {
      if (!checkParamCount(params)) Failure(AtomPropMatcherLibErrorParamCount(KnownPropMatcherParamCount, params.length))
      else {
        val regex = regexDict(params(0)(0))
        if (propName == texts) Success(textRegexMatcher(regex, exclude))
        else Success(propMatchRegex(propName, regex, exclude))
      }
    }
  }

  private val UnknownPropMatcherParamCount = 3
  private[TPropMatcherTmpl] class _UnknownPropEntityMatcher(val id:String, val matchType:PropMatchType, val caseSensi:Boolean, val exclude:Boolean)
    extends TPropMatcherTmpl {
    private[maen] def _expectedParamCount:Int = UnknownPropMatcherParamCount
    override def spawn(params:Array[Array[String]], regexDict:Map[String,String]):Try[TAtomMatcher] = {
      if (!checkParamCount(params)) Failure(AtomPropMatcherLibErrorParamCount(UnknownPropMatcherParamCount, params.length))
      else {
        val entityType = params(0)(0)
        val propName = params(1)(0)
        val matchers = Seq(AtomPropMatcherLib.E(regexDict, Array(entityType)), propMatchExact(propName, params(2), caseSensi, exclude))
        Success(composite(matchers))
      }
    }
  }

  private[TPropMatcherTmpl] class _UnknownPropEntityRegexMatcher(val id:String, val exclude:Boolean)
    extends TPropMatcherTmpl {
    private[maen] def _expectedParamCount:Int = UnknownPropMatcherParamCount
    override def spawn(params:Array[Array[String]], regexDict:Map[String,String]):Try[TAtomMatcher] = {
      if (!checkParamCount(params)) Failure(AtomPropMatcherLibErrorParamCount(UnknownPropMatcherParamCount, params.length))
      else {
        val entityType = params(0)(0)
        val propName = params(1)(0)
        val matchers = Seq(AtomPropMatcherLib.E(regexDict, Array(entityType)), propMatchRegex(propName, regexDict(params(2)(0)), exclude))
        Success(composite(matchers))
      }
    }
  }

  def KnownProp(id:String, propName:String, matchType:PropMatchType = AtLeastOne, caseSensi:Boolean = false, exclude:Boolean = false) =
    new _KnownPropMatcher(propName, id, matchType, caseSensi, exclude)
  def UnknownProp(id:String, matchType:PropMatchType = AtLeastOne, caseSensi:Boolean = false, exclude:Boolean = false) =
    new _UnknownPropEntityMatcher(id, matchType, caseSensi, exclude)
  def KnownPropRegex(id:String, propName:String, exclude:Boolean = false) =
    new _KnownPropRegexMatcher(propName, id, exclude)
  def UnknownPropRegex(id:String, exclude:Boolean = false) =
    new _UnknownPropEntityRegexMatcher(id, exclude)

}