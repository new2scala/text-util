package org.dele.text.maen

import org.dele.text.maen.AtomPropMatcherLib._
import org.dele.text.maen.ConfValueStringParser.Parsed
import org.dele.text.maen.TAtomMatcher.PropMatchBase
import org.dele.text.maen.matchers.MatcherManager._
import org.dele.text.maen.matchers.{StoppedByMatcherManager, SubMatchCheckerLib}
import org.dele.text.maen.test.TestAtom.Atom
import org.dele.text.maen.matchers.SubMatchCheckerLib
import org.dele.text.maen.test.TestInput

import scala.util.{Failure, Success, Try}

/**
  * Created by jiaji on 2016-02-08.
  */
object TestHelper {

  val HamletAnnotations2PropMatch:Map[String,String] = Map(
    "F" ->
      """
        |{
        | "texts":
        |}
        |""".stripMargin
  )

  import TestInput._
  def inputFrom(sentence:String):TInput = {
    val tokens = sentence.split("\\s+")
    fromAtomArrayEng(tokens.map(Atom(_, Map())).toIndexedSeq)
  }

  val EmptySubMatchCheckerLib = new SubMatchCheckerLib(List(), List())
  val StaticSubMatchCheckerLib = new SubMatchCheckerLib(List("Lng" -> Parsed("Lng", Array())), List())

  def DummyResultPool(input:TInput) = TMatchResultPool.create(input, EmptySubMatchCheckerLib)
}
