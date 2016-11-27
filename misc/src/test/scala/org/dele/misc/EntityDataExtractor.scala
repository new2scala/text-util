package org.dele.misc

import java.io.InputStream

import org.apache.commons.io.IOUtils
import org.dele.misc.EntityData.EntDetail


/**
  * Created by jiaji on 11/26/2016.
  */
object EntityDataExtractor extends App {

  import tgz.TgzUtil._
  val defaultEncoding = "UTF-8"

  def extractOne(in:InputStream):Map[String,EntDetail] = {
    val instr = IOUtils.toString(in, defaultEncoding)
    val entData = EntityData.Ser.p(instr)
    entData.entity_details.entMap
  }

  def extract(path:String):Map[String, EntDetail] = processAllFiles(path, extractOne).reduce(_ ++ _)

  private val datePartLength = 10
  def processGroupByDate(em:Map[String,EntDetail], days:Int) = {
    val dateGroups = em.groupBy(_._2.created.map(_.substring(0,datePartLength)))
    val sortedGroups = dateGroups.toIndexedSeq.sortBy(_._1)(Ordering[Option[String]].reverse).take(days)
    sortedGroups.foreach{ g =>
      println(s"${g._1} (${g._2.size})")
      val sorted = sortByCreatedDesc(g._2.values.toSeq)
      sorted.foreach(e => println(s"\t$e"))
    }
  }

  def sortByCreatedDesc(seq:Seq[EntDetail]):Seq[EntDetail] = seq.sortBy(_.created)(Ordering[Option[String]].reverse)

  def processBatch(em:Map[String,EntDetail], tag:String, latestCount:Int) = {
    val checkedEntities = em.toList.filter(_._2.curated == 1).toMap
    println("=====================================================\n")
    println(s"\n\n================== batch tag $tag ===================\n\n")
    println("=====================================================\n")
    println(s"Checked entity count: ${checkedEntities.size}")
    //val checkedByDate = checkedEntities.sortBy(_._2.created)(Ordering[Option[String]].reverse).take(20)
    processGroupByDate(checkedEntities, latestCount)
    //val uncheckedByDate = em.toIndexedSeq.sortBy(_._2.created)(Ordering[Option[String]].reverse).take(30)
    //println(checkedByDate.map(_._2).mkString("\n"))
    println("\n\n=====================================================\n\n")
    //println(uncheckedByDate.map(_._2).mkString("\n"))
    processGroupByDate(em, latestCount)
  }

  def checkAndCompare(path1:String, path2:String, latestCount:Int) = {
    val entMap1 = extract(path1)
    processBatch(entMap1, path1, latestCount)

    val entMap2 = extract(path2)
    processBatch(entMap2, path2, latestCount)

    val diff = (entMap1.toSet -- entMap2.toSet).toMap

    println("\n\n======================== Diff =======================\n\n")
    //  println(diff.map(_._2).mkString("\n"))
    processBatch(diff, "diff", latestCount)
  }

  checkAndCompare(
    "E:\\VMShare\\attack-vector-161127-21.tgz",
    "E:\\VMShare\\attack-vector-161126-12.tgz",
    10
  )
}
