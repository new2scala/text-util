package org.dele.text.lapa.patterns

import TLangPattern.{PtnNode, _LangPattern, LangPatternGroup}
import org.dele.text.maen.ConfValueStringParser
import org.dele.text.maen.ConfValueStringParser.Parsed
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization
import org.json4s.NoTypeHints

import scala.collection.mutable.ListBuffer

/**
  * Created by jiaji on 2016-08-19.
  */
import LangPatternGroupTemplate._

sealed class PtnNodeTemplate(val content:String, val isNeg:Option[Boolean]) {
  private[patterns] val _parsed:List[(String,Parsed)] = TLangPattern.parseNodeContent(content) //ConfValueStringParser.parseParam(content)
  //def isVar = _parsed.nonEmpty
  val (entryVarMap, paramVarMap) = buildVarMap(_parsed)


  //def instantiate(index:Int, param:Option[String] = None):PtnNode = new PtnNode(s"n$index", if (isVar) param.get else content, isNeg)
}

sealed class LangPatternTemplate(val nodes:List[PtnNodeTemplate], val defLineTypes:List[String]) {
  //def instantiate(id:String, nodes:List[PtnNode]):_LangPattern = new _LangPattern(Option(id), Set(), nodes, List())
}

sealed class LangPatternGroupTemplate(val languages:Set[String], val patterns:List[LangPatternTemplate]) {
  //private[LangPatternGroupTemplate] val _varMap:Map[PtnNodeTemplate, Int] = LangPatternGroupTemplate.buildVarMap(patterns)

  def instantiate(id:String, params:Array[Array[String]], defLineTypes:List[String], ext:Option[Boolean]):LangPatternGroup =
    instantiateGroup(this, id, params, defLineTypes, ext)
}

sealed class LangPatternGroupTemplateSet(val name:String, val templates:List[LangPatternGroupTemplate]) {

}

object LangPatternGroupTemplate {

  import scala.collection.mutable

  private def instantiateParamArray(params:Array[String]):String = params.mkString(ConfValueStringParser.SecondSeparatorChar)
  private def instantiateEntryParamArray(params:Array[String]):String = params.mkString("  ")

  def instantiateNode(index:Int, node:PtnNodeTemplate, params:Array[Array[String]]):PtnNode = {
    val entryIndexSet = mutable.Set[Int]()

    val t = node.entryVarMap.keys.map{ pidx =>
      val varIndices = node.entryVarMap.get(pidx).get
      entryIndexSet ++= varIndices
      varIndices.map(_ -> instantiateEntryParamArray(params(pidx)))
    }
    val newEntries:Set[(Int,String)] = if (t.nonEmpty) t.reduce(_ ++ _) else Set()
    val entries = newEntries ++ node._parsed.indices.filter(!entryIndexSet(_)).map(idx => idx -> node._parsed(idx)._1)

    //val paramIndexSet = mutable.Set[(Int,Int)]()
    val paraList = node._parsed.map(_._2.paras)
    node.paramVarMap.keys.foreach{ pidx =>
      val varIndices = node.paramVarMap.get(pidx).get
      varIndices.foreach{ idx =>
        val eidx = idx._1
        val x = idx._2
        val y = idx._3
        paraList(eidx)(x).update(y, instantiateParamArray(params(pidx)))
      }
    }
    val sorted = entries.toIndexedSeq.sortBy(_._1)
    val newParsed = sorted.map{ p =>
      Parsed(p._2, paraList(p._1))
    }

    val newContent = newParsed.map(_.unparse).mkString("  ")

    new PtnNode(s"n$index", newContent, node.isNeg)
  }


  def buildVarMap(parsed:List[(String, Parsed)]):(Map[Int,Set[Int]], Map[Int,Set[(Int,Int,Int)]]) = {
    val entryMap = mutable.Map[Int, Set[Int]]()
    val paramMap = mutable.Map[Int, Set[(Int,Int,Int)]]()
    parsed.indices.map { idx =>
      val p = parsed(idx)
      val ep = ConfValueStringParser.parseParam(p._1)
      if (ep.nonEmpty) {
        val paramIdx = ep.get-1
        if (!entryMap.contains(paramIdx)) entryMap += paramIdx -> Set()
        entryMap(paramIdx) += idx
      }
      p._2.paras.indices.foreach { xidx =>
        val parr = p._2.paras(xidx)
        parr.indices.foreach { yidx =>
          val pp = ConfValueStringParser.parseParam(parr(yidx))
          if (pp.nonEmpty) {
            val paramIdx = pp.get-1
            val coord = (idx,xidx,yidx)
            if (!paramMap.contains(paramIdx)) paramMap += paramIdx -> Set()
            paramMap(paramIdx) += coord
          }
        }
      }
    }
    entryMap.toMap -> paramMap.toMap
  }

  def instantiateGroup(template:LangPatternGroupTemplate, id:String, params:Array[Array[String]], defLineTypes:List[String], ext:Option[Boolean]):LangPatternGroup = {

    var ptnIdx = 0
    val ptns = template.patterns.map{ ptn =>
      ptnIdx += 1
      var idx = 0
      val nodes = ptn.nodes.map{ n =>
        idx += 1
        instantiateNode(idx, n, params)
      }
      val pattern = new _LangPattern(Option(s"p$ptnIdx"), Set(), nodes, List()) // ptn.instantiate(s"p$ptnIdx", nodes)
      nodes.foreach(_.init(pattern))
      pattern
    }
    TLangPattern.createPatternGroup(id, template.languages, ptns, defLineTypes, ext)
  }

  private def TemplateSet2Map(templateSet:LangPatternGroupTemplateSet):Map[String,LangPatternGroupTemplate] = {
    templateSet.templates.flatMap(tmpl => tmpl.languages.map(l => l -> tmpl)).toMap
  }
  sealed class LangPatternGroupTemplateLib(templateSets:Iterable[LangPatternGroupTemplateSet]) {

    private val _map = templateSets.map(ts => ts.name -> TemplateSet2Map(ts)).toMap
    def byNameAndLang(name:String, lang:String):Option[LangPatternGroupTemplate] = _map(name).get(lang)
  }

  val EmptyPatternTemplateLib = new LangPatternGroupTemplateLib(List())

  implicit val _formats = Serialization.formats(NoTypeHints)
  def parseJson(json:String):List[LangPatternGroupTemplateSet] = parse(json).extract[List[LangPatternGroupTemplateSet]]


}