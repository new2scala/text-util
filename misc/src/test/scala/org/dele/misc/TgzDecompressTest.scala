package org.dele.misc

import java.io.{BufferedInputStream, BufferedOutputStream, FileInputStream, FileOutputStream}

import org.apache.commons.compress.archivers.tar.TarArchiveInputStream
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream
import org.apache.commons.io.IOUtils

/**
  * Created by jiaji on 11/26/2016.
  */
object TgzDecompressTest extends App {
  val bfFileInputStream = new BufferedInputStream(new FileInputStream("E:\\VMShare\\malware-161126-12.tgz"))

  val tarIn = new TarArchiveInputStream(new GzipCompressorInputStream(bfFileInputStream))
  var tarEntry = tarIn.getNextEntry

  var tarEntryIdx = 0
  while (tarEntry != null) {
    val fileOrDir = if (tarEntry.isDirectory) "DIR" else "FILE"
    println(s"Extracting [${tarEntry.getName}]($fileOrDir)")

    if (!tarEntry.isDirectory) {
      val bfos = new BufferedOutputStream(new FileOutputStream(f"E:\\VMShare\\tmp\\$tarEntryIdx%04d.json"))
      val bufSize = 4096
      val buf = new Array[Byte](bufSize)
      var cnt = tarIn.read(buf, 0, bufSize)
      while (cnt != -1) {
        bfos.write(buf, 0, cnt)
        cnt = tarIn.read(buf, 0, bufSize)
      }
      bfos.close()
    }
    tarEntry = tarIn.getNextEntry
    tarEntryIdx = tarEntryIdx + 1
  }

  tarIn.close()
}
