package org.dele.misc.designPattern

/**
  * Created by jiaji on 12/2/2016.
  */
object AbstractFactoryTest extends App {
  trait TImage {
    def show:String
  }
  trait TImageReader {
    def decode(inFile: String):TImage
  }
  trait TImageTransformer {
    def transform(inFile: String):TImage
  }

  trait TImageToolBox {
    def getReader:TImageReader
    def getTransformer:TImageTransformer
  }

  class GifReader private[AbstractFactoryTest] extends TImageReader {
    override def decode(inFile: String): TImage = new TImage {
      override def show: String = s"This is a Gif from file [$inFile]"
    }
  }
  class GifTransformer private[AbstractFactoryTest] extends TImageTransformer {
    override def transform(inFile: String): TImage = new TImage {
      override def show: String = s"This is a TRANSFORMED Gif from file [$inFile]"
    }
  }
  class JpegReader private[AbstractFactoryTest] extends TImageReader {
    override def decode(inFile: String): TImage = new TImage {
      override def show: String = s"This is a JPEG from file [$inFile]"
    }
  }
  class JpegTransformer private[AbstractFactoryTest] extends TImageTransformer {
    override def transform(inFile: String): TImage = new TImage {
      override def show: String = s"This is a TRANSFORMED JPEG from file [$inFile]"
    }
  }

  private val JpegToolBoxInstance = new TImageToolBox {
    private val _reader = new JpegReader
    private val _transformer = new JpegTransformer

    override def getTransformer: TImageTransformer = _transformer
    override def getReader: TImageReader = _reader
  }
  private val _ToolBoxMapping:Map[String, TImageToolBox] = Map(
    "jpeg" -> JpegToolBoxInstance,
    "gif" -> new TImageToolBox {
      private val _reader = new GifReader
      private val _transformer = new GifTransformer
      override def getTransformer: TImageTransformer = _transformer
      override def getReader: TImageReader = _reader
    },
    "jpg" -> JpegToolBoxInstance
  )
  def getImageToolBoxFactoryFromFilePath(filePath:String):Option[TImageToolBox] = {
    val fileExt = filePath.substring(filePath.lastIndexOf('.')+1)
    _ToolBoxMapping.get(fileExt.toLowerCase)
  }

  val testFilePaths = List(
    "c:\\tmp\\a.jpg",
    "c:\\tmp\\b.jpeg",
    "c:\\tmp\\c.gif",
    "d:\\tmp\\e.gif"
  )

  testFilePaths.foreach{ fp =>
    val imgToolbox = getImageToolBoxFactoryFromFilePath(fp)
    println(s"Debug trace: ${imgToolbox}\t${imgToolbox.get.getReader}\t${imgToolbox.get.getTransformer}")
    val reader = imgToolbox.get.getReader
    println(reader.decode(fp).show)
    val transformer = imgToolbox.get.getTransformer
    println(transformer.transform(fp).show)
  }


}
