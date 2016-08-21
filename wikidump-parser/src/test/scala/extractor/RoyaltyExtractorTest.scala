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
          .withReign(0, List(Timeframe(Date("", "", "1285", true), Date("", "", "1291", true))))
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
          .withBirthDate(List(Date("4","November","1265", true)))
          // birth_place  = [[Valencia]]\n
          .withBirthPlace(List(Link("Valencia",None)))
          // death_date   = 18 June 1291 (aged 26)\n
          .withDeathDate(List(Date("18","June","1291", true)))
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
          .withReign(0, List(Timeframe(Date("2","April","1416", true), Date("27","June","1458", true))))
          .withReign(1, List(Timeframe(Date("2","June","1442", true), Date("27","June","1458", true))))
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
          .withDeathDate(List(Text("27 June"), Date("","","1458",true)))
          // death_place  = [[Castel dell'Ovo]], [[Naples]], [[Kingdom of Naples]]
          .withDeathPlace(List(Link("Castel dell'Ovo", None), Sep(","), Link("Naples", None), Sep(","), Link("Kingdom of Naples", None)))
          // religion     = [[Catholic Church|Roman Catholicism]]
          .withReligion(List(Link("Catholic Church", Some("Roman Catholicism"))))
          .build
      )
    )

    list.add(
      Array(
        "{{Infobox royalty\n| name = Alexander the Great\n| title = [[Basileus]] of [[Macedon]], [[Hegemony|Hegemon]] of the [[League of Corinth|Hellenic League]], [[Shahanshah]] of [[Persia]], [[Pharaoh]] of [[Ancient Egypt|Egypt]], [[Lord of Asia]]\n| image = Alexander the Great mosaic.jpg\n| caption = &lt;small&gt;{{Citation | contribution = Alexander fighting king [[Darius III of Persia]] | title = [[Alexander Mosaic]] | publisher = [[Naples National Archaeological Museum]]}}.&lt;/small&gt;\n| succession   = [[List of kings of Macedon|King of Macedonia]]\n| reign = 336–323 BC\n| predecessor  = [[Philip II of Macedon|Philip II]]\n| successor = [[Alexander IV of Macedon|Alexander IV]]&lt;br /&gt;[[Philip III of Macedon|Philip III]]\n| succession1   = [[Pharaoh|Pharaoh of Egypt]]\n| reign1 = 332–323 BC\n| predecessor1  = [[Darius III]]\n| successor1 = [[Alexander IV of Macedon|Alexander IV]]&lt;br /&gt;[[Philip III of Macedon|Philip III]]\n| succession2   = [[List of kings of Persia|King of Persia]]\n| reign2 = 330–323 BC\n| predecessor2  = [[Darius III]]\n| successor2 = [[Alexander IV of Macedon|Alexander IV]]&lt;br /&gt;[[Philip III of Macedon|Philip III]]\n| succession3   = [[Lord of Asia|King of Asia]]\n| reign3 = 331–323 BC\n| predecessor3  = ''New office''\n| successor3 = [[Alexander IV of Macedon|Alexander IV]]&lt;br /&gt;[[Philip III of Macedon|Philip III]]\n| othertitles =\n| full name = Alexander III of Macedon\n| native_lang1 = [[Greek language|Greek]]\n| native_lang1_name1 ={{plainlist |\n* Μέγας Ἀλέξανδρος{{Cref2|d}} (''Mégas Aléxandros'', Great Alexander)\n* Ἀλέξανδρος ὁ Μέγας (''Aléxandros ho Mégas'', Alexander the Great)\n}}\n| spouse = [[Roxana]] of [[Bactria]]&lt;br /&gt;[[Stateira II]] of [[Persia]]&lt;br /&gt;[[Parysatis II]] of Persia\n| issue = [[Alexander IV of Macedon|Alexander IV]]\n| house = [[Argead dynasty|Argead]]\n| house-type = Dynasty\n| father = [[Philip II of Macedon]]\n| mother = [[Olympias|Olympias of Epirus]]\n| birth_date = 20 or 21 July 356 BC\n| birth_place = [[Pella]], Macedon\n| death_date = 10 or 11 June 323 BC (aged 32)&lt;!-- 32 years, 10 months and 20 days (approx.) --&gt;\n| death_place = [[Babylon]]\n| religion = [[Religion in ancient Greece|Greek polytheism]]}}",
        new RoyaltyBuilder()
          // name         = Alexander the Great
          .withName(List(Text("Alexander the Great")))
          // succession   = [[List of kings of Macedon|King of Macedonia]]
          .withSuccession(0, List(Link("List of kings of Macedon", Some("King of Macedonia"))))
          // reign = 336–323 BC
          .withReign(0, List(Timeframe(Date("", "", "336", false), Date("", "", "323", false))))
          // predecessor  = [[Philip II of Macedon|Philip II]]
          .withPredecessor(0, List(Link("Philip II of Macedon", Some("Philip II"))))
          // successor = [[Alexander IV of Macedon|Alexander IV]]&lt;br /&gt;[[Philip III of Macedon|Philip III]]
          .withSuccessor(0, List(Link("Alexander IV of Macedon", Some("Alexander IV")), Link("Philip III of Macedon", Some("Philip III"))))
          // succession1   = [[Pharaoh|Pharaoh of Egypt]]
          .withSuccession(1, List(Link("Pharaoh", Some("Pharaoh of Egypt"))))
          // reign1 = 332–323 BC
          .withReign(1, List(Timeframe(Date("", "", "332", false), Date("", "", "323", false))))
          // predecessor1  = [[Darius III]]
          .withPredecessor(1, List(Link("Darius III", None)))
          // successor1 = [[Alexander IV of Macedon|Alexander IV]]&lt;br /&gt;[[Philip III of Macedon|Philip III]]
          .withSuccessor(1, List(Link("Alexander IV of Macedon", Some("Alexander IV")), Link("Philip III of Macedon", Some("Philip III"))))
          // succession2   = [[List of kings of Persia|King of Persia]]
          .withSuccession(2, List(Link("List of kings of Persia", Some("King of Persia"))))
          // reign2 = 330–323 BC
          .withReign(2, List(Timeframe(Date("", "", "330", false), Date("", "", "323", false))))
          // predecessor2  = [[Darius III]]
          .withPredecessor(2, List(Link("Darius III", None)))
          // successor2 = [[Alexander IV of Macedon|Alexander IV]]&lt;br /&gt;[[Philip III of Macedon|Philip III]]
          .withSuccessor(2, List(Link("Alexander IV of Macedon", Some("Alexander IV")), Link("Philip III of Macedon", Some("Philip III"))))
          // succession3   = [[Lord of Asia|King of Asia]]
          .withSuccession(3, List(Link("Lord of Asia", Some("King of Asia"))))
          // reign3 = 331–323 BC
          .withReign(3, List(Timeframe(Date("", "", "331", false), Date("", "", "323", false))))
          // predecessor3  = ''New office''
            .withPredecessor(3, List(Text("''New office''")))
          // successor3 = [[Alexander IV of Macedon|Alexander IV]]&lt;br /&gt;[[Philip III of Macedon|Philip III]]
          .withSuccessor(3, List(Link("Alexander IV of Macedon", Some("Alexander IV")), Link("Philip III of Macedon", Some("Philip III"))))
          // spouse = [[Roxana]] of [[Bactria]]&lt;br /&gt;[[Stateira II]] of [[Persia]]&lt;br /&gt;[[Parysatis II]] of Persia
          .withSpouse(List(Link("Roxana", None), Of("of"), Link("Bactria", None), Link("Stateira II", None), Of("of"), Link("Persia", None), Link("Parysatis II", None), Text("of Persia")))
          // issue = [[Alexander IV of Macedon|Alexander IV]]
          .withIssue(List(Link("Alexander IV of Macedon", Some("Alexander IV"))))
          // house = [[Argead dynasty|Argead]]
          .withHouse(List(Link("Argead dynasty", Some("Argead"))))
          // father = [[Philip II of Macedon]]
          .withFather(List(Link("Philip II of Macedon", None)))
          // mother = [[Olympias|Olympias of Epirus]]
          .withMother(List(Link("Olympias", Some("Olympias of Epirus"))))
          // birth_date = 20 or 21 July 356 BC
          .withBirthDate(List(Date("20 or 21","July","356",false)))
          // birth_place = [[Pella]], Macedon
          .withBirthPlace(List(Link("Pella", None), Text(", Macedon")))
          // death_date = 10 or 11 June 323 BC (aged 32)&lt;!-- 32 years, 10 months and 20 days (approx.) --&gt;
          .withDeathDate(List(Date("10 or 11","June","323",false)))
          // death_place = [[Babylon]]
          .withDeathPlace(List(Link("Babylon", None)))
          // religion = [[Religion in ancient Greece|Greek polytheism]]
          .withReligion(List(Link("Religion in ancient Greece", Some("Greek polytheism"))))
        .build
      )
    )

    list.add(
      Array(
        "{{Infobox royalty\n| birth_date    = {{birth date|1809|2|12}}}}",
        new RoyaltyBuilder()
          .withBirthDate(List(Date("12","2","1809",true)))
          .build
      )
    )

    list.add(
      Array(
        "{{Infobox royalty\n| birth_date    = {{birth date and age|1947|04|01|df=y}}}}",
        new RoyaltyBuilder()
          .withBirthDate(List(Date("01","04","1947",true)))
          .build
      )
    )

    list.add(
      Array(
        "{{Infobox royalty\n| birth_date    = {{Birth date|df=yes|1885|4|3}}}}",
        new RoyaltyBuilder()
          .withBirthDate(List(Date("3","4","1885",true)))
          .build
      )
    )

    list.add(
      Array(
        "{{Infobox royalty\n| birth_date    = 434–453 | death_date  = {{death date and age|df=yes|1670|2|9|1609|3|18}}}}",
        new RoyaltyBuilder()
          .withBirthDate(List(Timeframe(Date("", "", "434", true), Date("", "", "453", true))))
          .withDeathDate(List(Date("9","2","1670",true)))
          .build
      )
    )

    list.add(
      Array(
        "{{Infobox royalty\n| birth_date    = {{circa|268|232 BCE}}{{sfn|Upinder Singh|2008|p=331}} |death_date=February 14, 1009}}",
        new RoyaltyBuilder()
          .withBirthDate(List(Template("circa|268|232 BCE"), Template("sfn|Upinder Singh|2008|p=331")))
            .withDeathDate(List(Date("14","February","1009",true)))
          .build
      )
    )

    // non-parseable
//    list.add(
//      Array(
//        "{{Infobox royalty\n| birth_date    = 16 January 27 BC –&lt;br/&gt;19 August AD 14  &lt;small&gt;(40 years)&lt;/small&gt;}}",
//        new RoyaltyBuilder()
//          .withBirthDate(List(Text("16 January 27 BC – 19 August AD 14")))
//          .build
//      )
//    )

    list.add(
      Array(
        "{{Infobox royalty\n| birth_date    = c. 885 to 850 BC |death_date={{death date and age|df=yes|1621|9|17|1542|10|4}}&lt;br&gt;[[Rome]], Italy}}",
        new RoyaltyBuilder()
          .withBirthDate(List(Text("c. 885 to 850 BC")))
            .withDeathDate(List(Date("17","9","1621",true), Link("Rome", None), Text(", Italy")))
          .build
      )
    )

    list.add(
      Array(
        "{{Infobox royalty\n| birth_date    = {{nowrap|22 June 1691 – 6 February 1695}} |death_date={{death date|df=yes|814|2|18}}}}",
        new RoyaltyBuilder()
          .withBirthDate(List(Timeframe(Date("22","June","1691",true), Date("6","February","1695",true))))
          .withDeathDate(List(Date("18","2","814",true)))
          .build
      )
    )

    list.add(
      Array(
        "{{Infobox royalty\n| birth_date    = {{birth date|1912|4|26|mf=y}} | death_date  = {{death date and age|2001|05|13|1906|10|10|df=yes}}}}",
        new RoyaltyBuilder()
          .withBirthDate(List(Date("26","4","1912",true)))
          .withDeathDate(List(Date("13","05","2001",true)))
          .build
      )
    )

//    list.add(
//      Array(
//        "{{Infobox royalty\n| birth_date     = {{BirthDeathAge|B|1397| | |1475|12|10|yes}} | death_date     = {{BirthDeathAge| |1397| | |1475|12|10|yes}}}}",
//        new RoyaltyBuilder()
//          .withBirthDate(List(Date("26","4","1912",true)))
//          .withDeathDate(List(Date("13","05","2001",true)))
//          .build
//      )
//    )

//    list.add(
//      Array(
//        "{{Infobox royalty\n| birth_date = {{BirthDeathAge|B|1466|||1530}}| death_date = {{BirthDeathAge||1466|||1530}}}}",
//        new RoyaltyBuilder()
//          .withBirthDate(List(Date("26","4","1912",true)))
//          .withDeathDate(List(Date("13","05","2001",true)))
//          .build
//      )
//    )

//    list.add(
//      Array(
//        "{{Infobox royalty\n|birth_date        = {{BirthDeathAge|B|1932|4|27|1999|4|18|}}|death_date        = {{BirthDeathAge||1932|4|27|1999|4|18|}}}}",
//        new RoyaltyBuilder()
//          .withBirthDate(List(Date("26","4","1912",true)))
//          .withDeathDate(List(Date("13","05","2001",true)))
//          .build
//      )
//    )

    list
  }
}