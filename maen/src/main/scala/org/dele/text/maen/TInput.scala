package org.dele.text.maen

import org.dele.text.maen.matchers.TMatcher.MId
import org.dele.text.maen.matchers.TSubMatchChecker

/**
  * Created by jiaji on 2016-02-11.
  */
trait TInput {
  val lang:String
  def atoms:IndexedSeq[TAtom]
  //def subMatchChecker(checkerId:String):TSubMatchChecker
  val context:TContext
}
