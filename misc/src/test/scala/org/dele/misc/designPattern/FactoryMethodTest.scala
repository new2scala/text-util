package org.dele.misc.designPattern

import java.io.InputStream

/**
  * Created by jiaji on 12/2/2016.
  */
object FactoryMethodTest extends App {
  trait TImage {
    def show:String
  }
  trait TImageReader {
    def decode(inFile: String):TImage
  }

  class GifReader private[FactoryMethodTest] extends TImageReader {
    override def decode(inFile: String): TImage = new TImage {
      override def show: String = s"This is a Gif from file [$inFile]"
    }
  }

  class JpegReader private[FactoryMethodTest] extends TImageReader {
    override def decode(inFile: String): TImage = new TImage {
      override def show: String = s"This is a JPEG from file [$inFile]"
    }
  }

  private val JpegReaderInstance = new JpegReader
  private val _ReaderMapping:Map[String, TImageReader] = Map(
    "jpeg" -> JpegReaderInstance,
    "gif" -> new GifReader,
    "jpg" -> JpegReaderInstance
  )
  def getImageReaderFromFilePath(filePath:String):Option[TImageReader] = {
    val fileExt = filePath.substring(filePath.lastIndexOf('.')+1)
    _ReaderMapping.get(fileExt.toLowerCase)
  }

  val testFilePaths = List(
    "c:\\tmp\\a.jpg",
    "c:\\tmp\\b.jpeg",
    "c:\\tmp\\c.gif",
    "d:\\tmp\\e.gif"
  )

  testFilePaths.foreach{ fp =>
    val imgReader = getImageReaderFromFilePath(fp)
    println(s"Debug trace: $imgReader")
    println(imgReader.get.decode(fp).show)
  }

}
