package org.dele.text.maen.test

import org.dele.text.maen.matchers.TMatcher.MId
import org.dele.text.maen.utils.HamletError
import org.dele.text.maen._
import org.dele.text.maen.{ContextConstant, TAtom, TContext, TInput}
import org.dele.text.maen.utils.HamletError

/**
  * Created by jiaji on 2016-02-14.
  */
object TestInput {

  private[TestInput] class _Input(val lang:String, val atoms:IndexedSeq[TAtom], val context:TContext = ContextConstant.EmptyContext) extends TInput {
    def subMatchChecker(checkerId:String) = throw HamletError.NotImplemented
  }
  def fromAtomArray(lang:String, atoms:IndexedSeq[TAtom]):TInput = new _Input(lang, atoms)
  def fromAtomArrayEng(atoms:IndexedSeq[TAtom]):TInput = new _Input("eng", atoms)
}
