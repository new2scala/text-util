package org.dele.misc.tgz

import java.io._

import org.apache.commons.compress.archivers.tar.TarArchiveInputStream
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream

import scala.collection.mutable.ListBuffer

/**
  * Created by jiaji on 11/26/2016.
  */
object TgzUtil {
  type FileHandler[T] = InputStream => T

  def processAllFiles[T](path:String, handler:FileHandler[T]):List[T] = {
    val bfFileInputStream = new BufferedInputStream(new FileInputStream(path))

    val tarIn = new TarArchiveInputStream(new GzipCompressorInputStream(bfFileInputStream))
    var tarEntry = tarIn.getNextEntry

    var tarEntryIdx = 0
    val resultList = ListBuffer[T]()
    while (tarEntry != null) {
      val fileOrDir = if (tarEntry.isDirectory) "DIR" else "FILE"
      //println(s"Extracting [${tarEntry.getName}]($fileOrDir)")

      if (!tarEntry.isDirectory) {
        resultList += handler(tarIn)
        /*
        val bfos = new BufferedOutputStream(new FileOutputStream(f"E:\\VMShare\\tmp\\$tarEntryIdx%04d.json"))
        val bufSize = 4096
        val buf = new Array[Byte](bufSize)
        var cnt = tarIn.read(buf, 0, bufSize)
        while (cnt != -1) {
          bfos.write(buf, 0, cnt)
          cnt = tarIn.read(buf, 0, bufSize)
        }
        bfos.close()
        */
      }
      tarEntry = tarIn.getNextEntry
      tarEntryIdx = tarEntryIdx + 1
    }

    tarIn.close()
    resultList.toList
  }

  def processGzFile[T](path:String, handler:FileHandler[T]):T = {
    val bfFileInputStream = new BufferedInputStream(new FileInputStream(path))
    val gzIn = new GzipCompressorInputStream(bfFileInputStream)
    val result = handler(gzIn)
    gzIn.close()
    bfFileInputStream.close()
    result
  }
}
