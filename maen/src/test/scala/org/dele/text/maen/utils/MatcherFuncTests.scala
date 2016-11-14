package org.dele.text.maen.utils

import org.dele.text.maen.matchers.MatcherTmpl.MatcherFuncDef
import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test

/**
  * Created by jiaji on 2016-10-10.
  */
class MatcherFuncTests extends TestNGSuite with ShouldMatchers with TableDrivenPropertyChecks {

  val mfdef1 = "mf1($1|$2)"
  val mfdef2 = "mf1($2|$1)"
  val mfdef32 = "mf2(x|$2)"
  val mfdef3 = "mf1($2|const|$1)"
  val td = Table(
    ("defis", "input", "result"),
    (Array(mfdef1), Array(Array("a"), Array("b")), Array(Array(Array("a"), Array("b")))),
    //(mfdef1, Array("a", "b/c"), Array[Array[String]](Array("a"), Array("b", "c"))),
    //(mfdef2, Array("a/b", "c/d"), Array[Array[String]](Array("c", "d"), Array("a", "b"))),
    //(mfdef1, Array("9\\/11/8\\/14", "9/11"), Array[Array[String]](Array("9/11", "8/14"), Array("9", "11"))),
    //(mfdef1, Array("9\\/11 / 8\\/14", "9 / 11"), Array[Array[String]](Array("9/11", "8/14"), Array("9", "11"))),
    (Array(mfdef3), Array(Array("a", "b"), Array("c","d")), Array(Array(Array("c", "d"), Array("const"), Array("a", "b")))),
    (Array(mfdef3, mfdef32), Array(Array("a", "b"), Array("c","d")), Array(Array(Array("c", "d"), Array("const"), Array("a", "b")), Array(Array("x"), Array("c", "d")))),
    (Array(mfdef2), Array(Array("a"), Array("b")), Array(Array(Array("b"), Array("a"))))
  )
  @Test
  def t1 = {
    forAll(td) { (defis, input, result) =>
      val mf = new MatcherFuncDef("id1", defis)
      val rng = (0 until mf.templateCount)
      val params = rng.map(mf.getParams(_, input))
      rng.foreach(idx => params(idx) shouldBe result(idx))
    }

  }
}
