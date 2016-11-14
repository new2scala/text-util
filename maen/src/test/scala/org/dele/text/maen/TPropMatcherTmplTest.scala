package org.dele.text.maen

import org.dele.text.maen.test.TestAtom.Atom
import org.scalatest.ShouldMatchers
import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test

/**
  * Created by jiaji on 2016-02-09.
  */
class TPropMatcherTmplTest extends TestNGSuite with ShouldMatchers {
  import TPropMatcherTmpl._
  import TPropMatcherTmpl.PropMatchType._
  //import TestHelper.Atom
  @Test
  def t1 = {
    val kp = KnownProp("id1", "p1", AtLeastOne)

    val pm = kp.spawn(Array(Array("v1", "v2")), AtomPropMatcherLib.EmptyRegexDict)

    pm.isSuccess shouldBe(true)
    pm.get.check(Atom("txt", Map("p1" -> Set("v1", "v2")))) shouldBe(true)
  }


}
