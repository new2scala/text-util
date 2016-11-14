package org.dele.text.maen.utils

import org.scalatest._
import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test

/**
  * Created by jiaji on 2016-09-26.
  */
class Int2ByteArrayTest extends TestNGSuite with ShouldMatchers {
  import org.dele.text.maen.AtomSeqMatch._
  @Test
  def t1 = {
    var hu = int2Bytes(100)
    hu shouldBe Array[Byte](0, 0, 0, 0x64)
    hu = int2Bytes(0x10203000)
    hu shouldBe Array[Byte](0x10, 0x20, 0x30, 0)
  }

  @Test
  def t2 = {
    var di = md5Hash( Array[Byte](0, 0, 0, 0x64))
    di.length shouldBe 16

  }

  @Test
  def t21 = {
    var hash = md5Hash2Int(Array[Byte](1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))
    hash shouldBe(0x01060b10)

  }

  @Test
  def t3 = {
    val l1 = Array[Byte](0x10, 0x20, 0x30, 0)
    val l2 = Array[Byte](0x10, 0x21, 0x30, 0)
    val l3 = Array[Byte](0x10, 0x21, 0x20, 0)
    val l4 = Array[Byte](0x10, 0x20, 0x20, 0)

    ByteArrayOrdering.compare(l1, l1) shouldBe 0
    ByteArrayOrdering.compare(l1, l2) shouldBe -1
    ByteArrayOrdering.compare(l1, l3) shouldBe -1
    ByteArrayOrdering.compare(l1, l4) shouldBe 0x10

  }
}
