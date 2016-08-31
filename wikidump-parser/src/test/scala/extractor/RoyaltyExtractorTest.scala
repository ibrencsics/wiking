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
          .withBirthDate(List(Date("","","1396",true)))
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
      Array("""
{{Infobox monarch
|name            =Isaac II Angelos&lt;br/&gt;Ισαάκιος Β’ Άγγελος
|title           =[[Byzantine Emperor|Emperor]] of the [[Byzantine Empire]]
|image= Isaac_II_Angelos.jpg
|caption         =
|reign           =1185–1195&lt;br/&gt;1203–1204
|predecessor     =[[Andronikos I Komnenos]]
|successor       =[[Alexios III Angelos]]
|spouse 1        =?Herina
|spouse 2        =[[Margaret of Hungary]] (renamed Maria)
|issue           ={{plainlist|
* Euphrosyne Angelina,
* [[Irene Angelina]],
* [[Alexius IV Angelus|Alexios IV Angelos]],
* [[John Angelus of Syrmia|John Angelos]],
* Manuel Angelos
}}
|dynasty         =[[Angelus (dynasty)|Angelos dynasty]]
|father          =[[Andronikos Doukas Angelos]]
|mother          =Euphrosyne Kastamonitissa
|birth_date = {{birth-date|September 1156}}
|death_date = {{death-date|January 1204}}
}}""",
        new RoyaltyBuilder()
            .withName(List(Text("Isaac II Angelos"), Text("Ισαάκιος Β’ Άγγελος")))
            .withPredecessor(0, List(Link("Andronikos I Komnenos", None)))
            .withSuccessor(0, List(Link("Alexios III Angelos", None)))
            .withReign(0, List(Timeframe(Date("","","1185",true), Date("","","1195",true)), Timeframe(Date("","","1203",true), Date("","","1204",true))))
            .withSpouse(List(Text("?Herina"), Link("Margaret of Hungary", None), Text("(renamed Maria)")))
            .withIssue(List(People(List(Text("Euphrosyne Angelina"), Link("Irene Angelina", None), Link("Alexius IV Angelus", Some("Alexios IV Angelos")), Link("John Angelus of Syrmia", Some("John Angelos")), Text("Manuel Angelos")))))
            .withHouse(List(Link("Angelus (dynasty)", Some("Angelos dynasty"))))
            .withFather(List(Link("Andronikos Doukas Angelos", None)))
            .withMother(List(Text("Euphrosyne Kastamonitissa")))
            .withBirthDate(List(Date("","September","1156",true)))
            .withDeathDate(List(Date("","January","1204",true)))
          .build))


    list.add(
      Array("""
{{Infobox monarch
|name            = Basil I
|succession      = [[Byzantine Emperor]]
|image           = Solidus-Basil I with Constantine and Eudoxia-sb1703.jpg
|caption         = Basil, his son Constantine, and his second wife, Empress Eudokia Ingerina.
|reign           = 867–886
|predecessor     = [[Michael III]]
|successor       = [[Leo VI the Wise]]
|queen           = [[Eudokia Ingerina]]
|spouse 1        = Maria
|spouse 2        = [[Eudokia Ingerina]]
|issue           = [[Leo VI the Wise|Emperor Leo VI]]&lt;br&gt;[[Alexander (Byzantine emperor)|Emperor Alexander]]&lt;br&gt;[[Stephen I of Constantinople|Patriarch Stephen I]]
|issue-link      =#Family
|dynasty         = [[Macedonian dynasty]]
|father          =
|mother          =
|birth_date      = 811
|birth_place     = [[Macedonia (theme)|Macedonia]]
|death_date      = {{death-date|August 29, 886|August 29, 886}} (aged 75)
|death_place     =
|date of burial  =
|place of burial =
|}}""",
      new RoyaltyBuilder()
          .withName(List(Text("Basil I")))
          .withPredecessor(0, List(Link("Michael III", None)))
          .withSuccessor(0, List(Link("Leo VI the Wise", None)))
          .withReign(0, List(Timeframe(Date("","","867",true), Date("","","886",true))))
          .withSuccession(0, List(Link("Byzantine Emperor", None)))
          .withSpouse(List(Text("Maria"), Link("Eudokia Ingerina", None)))
          .withIssue(List(Link("Leo VI the Wise", Some("Emperor Leo VI")), Link("Alexander (Byzantine emperor)", Some("Emperor Alexander")), Link("Stephen I of Constantinople", Some("Patriarch Stephen I"))))
          .withHouse(List(Link("Macedonian dynasty", None)))
          .withFather(List())
          .withMother(List())
          .withBirthDate(List(Date("","","811",true)))
          .withBirthPlace(List(Link("Macedonia (theme)", Some("Macedonia"))))
          .withDeathDate(List(Date("29","August","886",true), Text("(aged 75)")))
          .withDeathPlace(List())
        .build))


    list.add(
      Array("""
{{Infobox person
| name             = Akira Kurosawa&lt;br/&gt;{{lang|ja|黒澤 明}}
| image            = Akirakurosawa-onthesetof7samurai-1953-page88.jpg
| caption          = Akira Kurosawa on the set of ''Seven Samurai'' in 1953
| parents          = Isamu Kurosawa&lt;br/&gt;Shima Kurosawa
| birth_date       = {{Birth date|1910|3|23}}
| birth_place      = [[Shinagawa]], Tokyo, Japan
| death_date       = {{Death date and age|1998|9|6|1910|3|23}}
| death_place      = [[Setagaya]], Tokyo, Japan
| death_cause      = Stroke
| occupation       = Director, screenwriter, producer, editor
| years_active     = 1936–93
| spouse           = [[Yōko Yaguchi]] (1945–85; her death)
| children         = [[Kazuko Kurosawa]]&lt;br&gt;Hisao Kurosawa
| relatives        = Yu Kurosawa (singer, granddaughter), Takayuki Kato (actor, grandson)}}
            """,
      new RoyaltyBuilder()
          .withName(List(Text("Akira Kurosawa"), Template("lang|ja|黒澤 明")))
          .withBirthDate(List(Date("23","3","1910",true)))
          .withBirthPlace(List(Link("Shinagawa", None), Text(", Tokyo, Japan"))) // postprocess
          .withDeathDate(List(Date("6","9","1998",true)))
          .withDeathPlace(List(Link("Setagaya", None), Text(", Tokyo, Japan")))
          .withSpouse(List(Link("Yōko Yaguchi", None), Text("(1945–85; her death)")))
          .withIssue(List(Link("Kazuko Kurosawa", None), Text("Hisao Kurosawa")))
          .build))

/*
{{Infobox person
 | name             = Ada, Countess of Lovelace
 | image            = Ada Lovelace portrait.jpg
 | caption          = Ada, Countess of Lovelace, 1840
 | birth_name       = The Hon. Augusta Ada Byron
|birth_date={{birth date|1815|12|10|df=yes}}
 | birth_place      = London, England
 | death_date       = {{death date and age|1852|11|27|1815|12|10|df=yes}}
 | death_place      = [[Marylebone]], London, England
 | resting_place    = [[Church of St. Mary Magdalene, Hucknall]], Nottingham, England
 | title            = Countess of Lovelace
| children         =
  {{plainlist |
* [[Byron King-Noel, Viscount Ockham|Byron King-Noel, Viscount Ockham and 12th Baron Wentworth]]
* [[Anne Blunt, 15th Baroness Wentworth]]
* [[Ralph King-Milbanke, 2nd Earl of Lovelace]]}}
 | parents          =
  {{plainlist |
* [[Lord Byron|George Gordon Byron, 6th Baron Byron]]
* [[Anne Isabella Byron, Baroness Byron|Anne Isabella Milbanke, 11th Baroness Wentworth]]
  }}
 | known for           = [[Mathematics]]&lt;br&gt;[[Computing]]
 | spouse           = [[William King-Noel, 1st Earl of Lovelace]]
 }}



     */



// |reign-type      = 1st reign
// |reign           = 13 September 1438 – &lt;br&gt; 11 November 1477
// |reign-type1     = 2nd reign
// |reign1          = 15 November 1477 – &lt;br&gt; 28 August 1481

    list
  }
}