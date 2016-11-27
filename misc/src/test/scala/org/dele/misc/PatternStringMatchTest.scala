package org.dele.misc

import org.joda.time.DateTime
import org.scalatest.{FlatSpec, Matchers}

import scala.util.matching.Regex

/**
  * Created by jiaji on 11/27/2016.
  */
class PatternStringMatchTest extends FlatSpec with Matchers {
  "String pattern match" should "ok" in {


    val dateTimePattern = """(\d{4})-(\d{2})-(\d{2})T(\d{2}):(\d{2}):(\d{2})\.(\d{3})Z""".r
    val dateTimePattern(year, month, day, hour, minute, second, millisecond) = "2015-11-04T17:00:22.970Z"
    val dt = new DateTime(year.toInt, month.toInt, day.toInt, hour.toInt, minute.toInt, second.toInt, millisecond.toInt)
    dt shouldNot be(null)

  }
}
