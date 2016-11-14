package org.dele.text.maen

import org.dele.text.maen.utils.HamletError
import org.dele.text.maen.utils.HamletError.HamletErrorBase
import org.dele.text.maen.matchers.TMatcher

/**
  * Created by jiaji on 2016-02-05.
  */

object ErrorHandling {

  val MatchCheckerErrorNoSubMatches = new HamletErrorBase("No sub-matches found. Checking cannot be performed!")

  final case class NotImplemented(msg:String)
    extends HamletErrorBase(s"Not Implemented: $msg")

  final case class ExtractBlockErrorIllFormattedDef(extractBlockDef:String)
    extends HamletErrorBase("Unrecognizable extract block definition: %s".format(extractBlockDef))

  final case class MatcherErrorNoConsecutiveNegMatchers(negIndices:IndexedSeq[Int])
    extends HamletErrorBase("Consecutive negative sub-matches (indices: %s) not allowed (doesn't make sense).".format(negIndices))

  final case class MatcherManagerErrorMatcherIdAlreadyExist(existing:TMatcher, toAdd:TMatcher)
    extends HamletErrorBase("Matcher with id(%s) already exist: [%s], cannot add another [%s] with the same id".format(existing.id, existing, toAdd))

  final case class ExtractErrorJSONFormat(jsonObj:AnyRef)
    extends HamletErrorBase("Unable to deserialize: %s".format(jsonObj))

  final case class SubMatchCheckerLibErrorUnknownTemplate(templateId:String)
    extends HamletErrorBase("Unknown template: %s".format(templateId))

  final case class SubMatchCheckerErrorUnknownCheckerId(checkerId:String)
    extends HamletErrorBase("Unknown checker Id: %s".format(checkerId))

  final case class MatcherTemplateErrorSpawnMatcher(tmplId:String, msg:String)
    extends HamletErrorBase(s"Template [$tmplId] spawning error: $msg")

  final case class AtomErrorNoProp(propName:String)
    extends HamletErrorBase("Property [%s] not present.".format(propName))

  final case class MatchCheckerInputDiffer(m1:AtomSeqMatch, m2:AtomSeqMatch)
    extends HamletErrorBase("Matches are from different input: [%s] vs. [%s]".format(m1, m2))

  final case class AtomErrorMultvalueProp(propName:String,values:Set[String])
    extends HamletErrorBase("Property [%s] is not a single-valued property (values:[%s], call propValues instead.".format(propName, values))

  final case class AtomPropMatcherLibErrorParamCount(expected:Int, actual:Int)
    extends HamletErrorBase("Parameter count does not match: expected [%d], actual [%d].".format(expected, actual))

  final case class AtomPropMatcherLibErrorParamType(expected:String, actual:String)
    extends HamletErrorBase("Parameter type does not match: expected [%s], actual [%s].".format(expected, actual))

  final case class AtomPropMatcherErrorDefinition(matcherDef:String)
    extends HamletErrorBase("Unrecognized property matcher definition: [%s]".format(matcherDef))
}
