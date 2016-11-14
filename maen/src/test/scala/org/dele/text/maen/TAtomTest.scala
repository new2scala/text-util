package org.dele.text.maen

import org.dele.text.maen.test.TestAtom.Atom
import org.dele.text.maen.utils.HamletError
import org.dele.text.maen.utils.HamletError
import org.scalatest.ShouldMatchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.Tables.Table
import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test

import scala.util.Failure

/**
  * Created by jiaji on 2016-02-05.
  */
class TAtomTest extends TestNGSuite with ShouldMatchers with TableDrivenPropertyChecks {

  import ErrorHandling._
  //import TestHelper.Atom

  def newMap(text:String, vset:Set[String]) = Map[String,Set[String]](text -> vset)



  val atomCreationTable = Table(
    ("text", "propMap"),
    ("txt1", newMap("prop1", Set("v1", "v2"))),
    ("txt2", newMap("prop2", Set[String]()))
  )

  @Test
  def applyTest = {
    forAll(atomCreationTable) {
      (text, propMap) => {
        val atom = Atom(text, propMap)

        atom.text shouldBe(text)
        propMap.keySet.foreach(
          k => {
            propMap.get(k).get shouldBe(atom.propValues(k).get)
          }
        )
      }
    }
  }

  @Test
  def propValueTest2 = {
    forAll(atomCreationTable) {
      (text, propMap) => {
        val atom = Atom(text, propMap)

        atom.text shouldBe(text)
        propMap.keySet.foreach(
          k => {
            val v = atom.propValue(k)
            v.isFailure shouldBe(true)
            v match {
              case Failure(x) => {
                x match {
                  case e:HamletError => println(e.describe)
                  case xx => fail("Expects AtomErrorMultvalueProp, but get %s instead".format(xx.getClass))
                }
              }
              case _ => fail("Success result not expected")
            }
          }
        )
      }
    }
  }
}
