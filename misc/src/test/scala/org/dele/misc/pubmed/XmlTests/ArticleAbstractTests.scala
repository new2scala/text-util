package org.dele.misc.pubmed.XmlTests

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by dele on 2017-02-25.
  */
class ArticleAbstractTests extends FlatSpec with Matchers with TableDrivenPropertyChecks {
  val abs_m =
    """
      |<Abstract>
      |          <AbstractText Label="OBJECTIVE" NlmCategory="OBJECTIVE">Patient expectancies are hypothesized to contribute to the efficacy and side effects of psychiatric treatments, but little research has investigated this hypothesis in the context of psychopharmacological therapies for anxiety. We prospectively investigated whether expectancies predicted efficacy and adverse events in oral therapy for Generalized Anxiety Disorder (GAD), controlling for confounding patient characteristics correlating with outcomes.</AbstractText>
      |          <AbstractText Label="METHODS" NlmCategory="METHODS">Expectancies regarding treatment efficacy and side effects were assessed at baseline of an eight week open-label phase of a trial of chamomile for Generalized Anxiety Disorder (GAD). The primary outcome was patient-reported GAD-7 scores, with clinical response and treatment-emergent side-effects as secondary outcomes. Expectancies were used to predict symptomatic and side-effect outcomes.</AbstractText>
      |          <AbstractText Label="RESULTS" NlmCategory="RESULTS">Very few baseline patient characteristics predicted either type of expectancy. Controlling for a patient's predicted recovery based on their baseline characteristics, higher efficacy expectancies at baseline predicted greater change on the GAD-7 (adjusted β = -0.19, p = 0.011). Efficacy expectancies also predicted a higher likelihood of attaining clinical response (adjusted odds ratio = 1.69, p = 0.002). Patients with higher side effect expectancies reported more side effects (adjusted log expected count = 0.26, p = 0.038). Efficacy expectancies were unrelated to side effect reports (log expected count = -0.05, p = 0.680), and side effect expectancies were unrelated to treatment efficacy (β = 0.08, p = 0.306).</AbstractText>
      |          <AbstractText Label="CONCLUSIONS" NlmCategory="CONCLUSIONS">Patients entering chamomile treatment for GAD with more favorable self-generated expectancies for the treatment experience greater improvement and fewer adverse events. Aligning patient expectancies with treatment selections may optimize outcomes.</AbstractText>
      |          <AbstractText Label="REGISTRATION" NlmCategory="UNASSIGNED">Trial Number NCT01072344 at ClinicalTrials.gov.</AbstractText>
      |          <CopyrightInformation>Copyright Â© 2016 Elsevier Ltd. All rights reserved.</CopyrightInformation>
      |</Abstract>
      |
    """.stripMargin
  val abs_1 =
    """
      |<Abstract>
      |          <AbstractText>Understanding human embryonic ventral midbrain is of major interest for Parkinson's disease. However, the cell types, their gene expression dynamics, and their relationship to commonly used rodent models remain to be defined. We performed single-cell RNA sequencing to examine ventral midbrain development in human and mouse. We found 25 molecularly defined human cell types, including five subtypes of radial glia-like cells and four progenitors. In the mouse, two mature fetal dopaminergic neuron subtypes diversified into five adult classes during postnatal development. Cell types and gene expression were generally conserved across species, but with clear differences in cell proliferation, developmental timing, and dopaminergic neuron development. Additionally, we developed a method to quantitatively assess the fidelity of dopaminergic neurons derived from human pluripotent stem cells, at a single-cell level. Thus, our study provides insight into the molecular programs controlling human midbrain development and provides a foundation for the development of cell replacement therapies.</AbstractText>
      |          <CopyrightInformation>Copyright © 2016 The Author(s). Published by Elsevier Inc. All rights reserved.</CopyrightInformation>
      |</Abstract>
    """.stripMargin
  val testData = Table(
    ("xml", "absTextCount", "firstLabel", "firstNlmCategory", "firstText"),
    (abs_m, 5, Option("OBJECTIVE"), Option("OBJECTIVE"), "Patient expectancies are hypothesized to contribute to the efficacy and side effects of psychiatric treatments, but little research has investigated this hypothesis in the context of psychopharmacological therapies for anxiety. We prospectively investigated whether expectancies predicted efficacy and adverse events in oral therapy for Generalized Anxiety Disorder (GAD), controlling for confounding patient characteristics correlating with outcomes."),
    (abs_1, 1, None, None, "Understanding human embryonic ventral midbrain is of major interest for Parkinson's disease. However, the cell types, their gene expression dynamics, and their relationship to commonly used rodent models remain to be defined. We performed single-cell RNA sequencing to examine ventral midbrain development in human and mouse. We found 25 molecularly defined human cell types, including five subtypes of radial glia-like cells and four progenitors. In the mouse, two mature fetal dopaminergic neuron subtypes diversified into five adult classes during postnatal development. Cell types and gene expression were generally conserved across species, but with clear differences in cell proliferation, developmental timing, and dopaminergic neuron development. Additionally, we developed a method to quantitatively assess the fidelity of dopaminergic neurons derived from human pluripotent stem cells, at a single-cell level. Thus, our study provides insight into the molecular programs controlling human midbrain development and provides a foundation for the development of cell replacement therapies.")
  )

  import org.dele.misc.pubmed.PubmedXmlHelpers._
  "abstract count/text/label" should "match" in {
    forAll(testData) { (xml, absTextCount, firstLabel, firstNlmCategory, firstText) =>
      val n = scala.xml.XML.loadString(xml)
      val _abs = xml2Abstract(n)
      _abs.nonEmpty shouldBe true
      val abs = _abs.get
      abs.AbstractText.size shouldBe absTextCount
      val abs1 = abs.AbstractText.head
      abs1.Label shouldBe firstLabel
      abs1.NlmCategory shouldBe firstNlmCategory
      abs1.Text shouldBe firstText
    }
  }
}
