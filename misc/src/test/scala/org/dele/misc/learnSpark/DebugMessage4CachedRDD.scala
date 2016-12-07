package org.dele.misc.learnSpark

/**
  * Created by jiaji on 12/4/2016.
  */
object DebugMessage4CachedRDD extends App {
  import SparkTestUtil._
  val rdd = context.textFile("LICENSE")

  rdd.cache()

  // debug message:
  // 16/12/04 11:52:35 INFO HadoopRDD: Input split: file:/E:/Projs/text-util/text-util/LICENSE:5779+5779
  // 16/12/04 11:52:35 INFO HadoopRDD: Input split: file:/E:/Projs/text-util/text-util/LICENSE:0+5779
  val totalLength = rdd.map(l => l.length).sum
  //16/12/04 11:52:35 INFO MemoryStore: Block rdd_1_0 stored as values in memory (estimated size 15.4 KB, free 1994.9 MB)
  //16/12/04 11:52:35 INFO BlockManagerInfo: Added rdd_1_0 in memory on 84.219.204.129:61619 (size: 15.4 KB, free: 1995.0 MB)
  //16/12/04 11:52:35 INFO MemoryStore: Block rdd_1_1 stored as values in memory (estimated size 15.1 KB, free 1994.8 MB)
  //16/12/04 11:52:35 INFO BlockManagerInfo: Added rdd_1_1 in memory on 84.219.204.129:61619 (size: 15.1 KB, free: 1995.0 MB)

  val wordCount = rdd.map(l => l.trim.split("\\s+").length).sum

  // 16/12/04 11:56:07 INFO BlockManager: Found block rdd_1_0 locally
  // 16/12/04 11:56:07 INFO BlockManager: Found block rdd_1_1 locally
  println(s"Totol doc length: $totalLength; word count: $wordCount")
  println("done")
}
