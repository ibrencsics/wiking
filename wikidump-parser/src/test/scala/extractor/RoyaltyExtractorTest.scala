package extractor

import java.{util => ju, lang => jl}

import org.junit.runner.RunWith
import org.junit.runners.Parameterized.Parameters
import org.scalatest.junit.JUnitSuite
import org.junit.Test
import org.junit.Assert._
import org.junit.runners.Parameterized


@RunWith(value = classOf[Parameterized])
class RoyaltyExtractorTest(p1: Object, p2: Object) extends JUnitSuite {

  @Test def verify(): Unit = {
    val royalty = new RoyaltyExtractor().extract(p1.asInstanceOf[String])
    val expRoyalty: Royalty = p2.asInstanceOf[Royalty]

//    assertTrue(royalty == expRoyalty)
    assertEquals(expRoyalty, royalty)

//    println(royalty)
  }
}

object RoyaltyExtractorTest {

  @Parameters def parameters: ju.Collection[Array[Object]] = {
    val list = new ju.ArrayList[Array[Object]]()

    list.add(
      Array(
        "{{infobox royalty\\n| name         = Alfonso III\\n| image        = Jaume Mateu - Alfons III the Liberal - Google Art Project.jpg\\n| succession   = [[King of Aragon]], [[King of Valencia|Valencia]] and [[Count of Barcelona]] \\n| reign        = 1285–1291\\n| coronation   = 2 February 1286 (Valencia)&lt;br /&gt;9 April 1286 (Zaragoza)\\n| predecessor  = [[Peter III of Aragon|Peter III]]\\n| successor     = [[James II of Aragon|James II]]\\n| spouse       = \\n| issue        = \\n| house        = [[House of Barcelona]]\\n| father       = [[Peter III of Aragon]]\\n| mother       = [[Constance of Sicily, Queen of Aragon|Constance of Sicily]]\\n| birth_date   = 4 November 1265\\n| birth_place  = [[Valencia]]\\n| death_date   = 18 June 1291 (aged 26)\\n| death_place  = [[Barcelona]]\\n| burial_date  =\\n| burial_place = [[Barcelona Cathedral]]; prev. Convent de San Francisco, Barcelona\\n| religion     = [[Catholic Church|Roman Catholicism]]\\n}}",
        new RoyaltyBuilder()
          // name         = Alfonso III\n
          .withName(List(Text("Alfonso III")))
          // succession   = [[King of Aragon]], [[King of Valencia|Valencia]] and [[Count of Barcelona]] \n
          .withSuccession(0, List(Link("King of Aragon",None), Sep(","), Link("King of Valencia",Some("Valencia")), Sep("and"), Link("Count of Barcelona",None)))
          // reign        = 1285–1291\n
          .withReign(0, List(Text("1285–1291")))
          // predecessor  = [[Peter III of Aragon|Peter III]]\n
          .withPredecessor(0, List(Link("Peter III of Aragon",Some("Peter III"))))
          // successor     = [[James II of Aragon|James II]]\n
          .withSuccessor(0, List(Link("James II of Aragon",Some("James II"))))
          // spouse       = \n
          .withSpouse(List())
          // issue        = \n
          .withIssue(List())
          // house        = [[House of Barcelona]]\n
          .withHouse(List(Link("House of Barcelona",None)))
          // father       = [[Peter III of Aragon]]\n
          .withFather(List(Link("Peter III of Aragon",None)))
          // mother       = [[Constance of Sicily, Queen of Aragon|Constance of Sicily]]\n
          .withMother(List(Link("Constance of Sicily, Queen of Aragon",Some("Constance of Sicily"))))
          // birth_date   = 4 November 1265\n
          .withBirthDate(List(Text("4 November 1265")))
          // birth_place  = [[Valencia]]\n
          .withBirthPlace(List(Link("Valencia",None)))
          // death_date   = 18 June 1291 (aged 26)\n
          .withDeathDate(List(Text("18 June 1291 (aged 26)")))
          // death_place  = [[Barcelona]]\n
          .withDeathPlace(List(Link("Barcelona",None)))
          // religion     = [[Catholic Church|Roman Catholicism]]\n
          .withReligion(List(Link("Catholic Church",Some("Roman Catholicism"))))
          .build
      )
    )

    list.add(
      Array(
        "{{Infobox royalty\\n| type         = monarch\\n| name         = Alfonso the Magnanimous\\n| image        = Alfonso-V-el-Magnanimo.jpg\\n| caption      = Portrait of Alfonso V of Aragon, by 16th century painter [[Vicente Juan Masip]]\\n| spouse       = [[Maria of Castile]]\\n| succession   = [[Crown of Aragon|King of Aragon]]\\n| reign        = 2 April 1416 – 27 June 1458\\n| predecessor  = [[Ferdinand I of Aragon|Ferdinand I]]\\n| successor    = [[John II of Aragon|John II]]\\n| succession1  = [[List of monarchs of Naples|King of Naples]] [[List of monarchs of Sicily|and Sicily]]\\n| reign1       = 2 June 1442 – 27 June 1458\\n| predecessor1 = [[René of Anjou|René]]\\n| successor1   = [[Ferdinand I of Naples|Ferdinand I]]\\n| issue        = [[Ferdinand I of Naples]] &lt;small&gt;(illegitimate)&lt;/small&gt;\\n| issue-link   = #Family\\n| issue-pipe   = among others...\\n| house        = [[House of Trastámara]]\\n| father       = [[Ferdinand I of Aragon]]\\n| mother       = [[Eleanor of Alburquerque]]\\n| birth_date   = 1396\\n| birth_place  = [[Medina del Campo]], [[Kingdom of Castile]]\\n| death_date   = 27 June {{death year and age|df=yes|1458|1396}}\\n| death_place  = [[Castel dell'Ovo]], [[Naples]], [[Kingdom of Naples]]\\n| burial_date  = \\n| burial_place = [[Poblet Monastery]]\\n| religion     = [[Catholic Church|Roman Catholicism]]\\n}}",
        new RoyaltyBuilder()
          // name         = Alfonso the Magnanimous
          .withName(List(Text("Alfonso the Magnanimous")))
          // succession   = [[Crown of Aragon|King of Aragon]]
          // succession1  = [[List of monarchs of Naples|King of Naples]] [[List of monarchs of Sicily|and Sicily]]
          .withSuccession(0, List(Link("Crown of Aragon", Some("King of Aragon"))))
          .withSuccession(1, List(Link("List of monarchs of Naples", Some("King of Naples")), Link("List of monarchs of Sicily", Some("and Sicily"))))
          // predecessor  = [[Ferdinand I of Aragon|Ferdinand I]]
          // predecessor1 = [[René of Anjou|René]]
          .withPredecessor(0, List(Link("Ferdinand I of Aragon", Some("Ferdinand I"))))
          .withPredecessor(1, List(Link("René of Anjou", Some("René"))))
          // successor    = [[John II of Aragon|John II]]
          // successor1   = [[Ferdinand I of Naples|Ferdinand I]]
          .withSuccessor(0, List(Link("John II of Aragon", Some("John II"))))
          .withSuccessor(1, List(Link("Ferdinand I of Naples", Some("Ferdinand I"))))
          // reign        = 2 April 1416 – 27 June 1458
          // reign1       = 2 June 1442 – 27 June 1458
          .withReign(0, List(Text("2 April 1416 – 27 June 1458")))
          .withReign(1, List(Text("2 June 1442 – 27 June 1458")))
          // spouse       = [[Maria of Castile]]
          .withSpouse(List(Link("Maria of Castile", None)))
          // issue        = [[Ferdinand I of Naples]] &lt;small&gt;(illegitimate)&lt;/small&gt;
          .withIssue(List(Link("Ferdinand I of Naples", None), Text("(illegitimate)")))
          // house        = [[House of Trastámara]]
          .withHouse(List(Link("House of Trastámara", None)))
          // father       = [[Ferdinand I of Aragon]]
          .withFather(List(Link("Ferdinand I of Aragon", None)))
          // mother       = [[Eleanor of Alburquerque]]
          .withMother(List(Link("Eleanor of Alburquerque", None)))
          // birth_date   = 1396
          .withBirthDate(List(Text("1396")))
          // birth_place  = [[Medina del Campo]], [[Kingdom of Castile]]
          .withBirthPlace(List(Link("Medina del Campo", None), Sep(","), Link("Kingdom of Castile", None)))
          // death_date   = 27 June {{death year and age|df=yes|1458|1396}}
          .withDeathDate(List(Text("27 June {{death year and age|df=yes|1458|1396}}")))
          // death_place  = [[Castel dell'Ovo]], [[Naples]], [[Kingdom of Naples]]
          .withDeathPlace(List(Link("Castel dell'Ovo", None), Sep(","), Link("Naples", None), Sep(","), Link("Kingdom of Naples", None)))
          // religion     = [[Catholic Church|Roman Catholicism]]
          .withReligion(List(Link("Catholic Church", Some("Roman Catholicism"))))
          .build
      )
    )

    list
  }
}