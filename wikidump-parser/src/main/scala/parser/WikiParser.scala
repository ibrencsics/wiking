package parser

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

sealed trait Element
case class Link(page: String, plain: Option[String]) extends Element
case class Text(plain: String) extends Element
case class Sep(data: String) extends Element
case class FreeText(data: List[Element])

case class Template(name: String, data: ListBuffer[String])
case class Infobox(name: String, data: List[(String, String)])


trait Base {
  def raw: String
  def parsed: FreeText
}

case class Succession(val raw: String, val parsed: FreeText) extends Base
case class Successor(val raw: String, val parsed: FreeText) extends Base
case class Predecessor(val raw: String, val parsed: FreeText) extends Base
case class Reign(val raw: String, val parsed: FreeText) extends Base
case class Job(val succession: Succession, val predecessor: Predecessor, val successor: Successor, val reign: Reign)

case class Royalty(
//  title: Option[String],
  job: List[Job])

class WikiParser {

  def parseRoyalty(text: String): Option[Royalty] = {
    val successions = mutable.Map[Int, Succession]()
    val predecessors = ListBuffer[Predecessor]()

    parseInfobox(text)
      .filter(i => i.name == "royalty")
      .map(i =>
//        .map(i => Royalty(
//          i.data.find(_._1.startsWith("title")).map(_._2),            // TODO: occupation
          i.data
            .filter(_._1.startsWith("succession"))
            .foreach(s => successions += getPostfix(s._1) -> Succession(s._2, parseFreeText(s._2))
//            .map(s => {
//              val parsed = parseFreeText(s._2)
//              Job(Succession(s._2, parsed), null, null, null)
//            })
          )
        )

    Some( Royalty( successions.map(s => Job(s._2, null, null, null)).toList ) )
  }

  def getPostfix(s: String): Int = {
    import scala.language.postfixOps
    val postfix = """\w+(d*)""".r.findAllIn(s).group(1)
    if (postfix isEmpty) 0 else postfix.toInt
  }

  def parseFreeText(raw: String): FreeText = {
    val elems = ListBuffer[Element]()

    var pos = 0

    var linkLevel = 0
    var linkStart = -1
    var linkPresent = false

    var templateLevel = 0
    var templateStart = -1
    var templatePresent = false

    var buf = ArrayBuffer[String]()

    for (c: Char <- raw) {
      if (c equals '[') {
//        println("'[' found")

        if (buf.nonEmpty) {
          val text = buf.mkString

          if (text.trim == ",") {
            elems += Sep(text.trim)
          } else {
            elems += Text(buf.mkString)
          }

          buf.clear()
        }

        linkLevel += 1
      }
      else if (c equals ']') {
//        println("']' found")
        linkLevel -= 1
      }

      if (!linkPresent && linkLevel == 2) {
        linkPresent = true
        linkStart = pos + 1
//        println(s"link start, linkStart = ${linkStart}")
      }
      else if (linkPresent && linkLevel == 0) {
        val link = raw.substring(linkStart, pos - 1)
        val split = link.split("\\|")
        elems += Link(split(0), if (split.size > 1) Option(split(1)) else Option.empty)
        linkPresent = false
      }
      else if (!linkPresent && !c.equals('[') && !c.equals(']')) {
        buf += c.toString
      }

//      println(s"linklevel = ${linkLevel}, linkPresent = ${linkPresent}, linkStart = ${linkStart}")
      pos += 1
    }

    FreeText(elems.result)
  }

  def parseInfobox(text: String): Option[Infobox] = {
    def splitLine(line: String): (String, String) = {
      val index = line.indexOf('=')
      try {
        (line.substring(0, index).trim, line.substring(index + 1).trim)
      } catch {
        case e: Exception => ("", "")
      }
    }

    def cleanup(raw: String): String = {
      raw
        .replaceAll("br\\s*/", ",")
        .replaceAll("small\\(", "(")
        .replaceAll("/small", "")
    }

    val template = parseTemplate(text)

    val pattern = """[Ii]nfobox\s*"""

    pattern.r.findFirstMatchIn(template.name).map(_ =>
      Infobox(
        template.name.replaceAll(pattern, ""),
        template.data.map(splitLine(_)).toList
      )
    )
  }

  def parseTemplate(text: String): Template = {

    var level = 0
    var pos = 0
    var startPos = -1

    var name = "unknown"
    val data = new ListBuffer[String]()

    val processedText = text.replaceAll("\\n", "")

    for (c: Char <- processedText) {
      if (c == '{' || c == '[') level += 1
      else if (c == '}' || c == ']') level -= 1

      if (c == '|' && level==2) {
        if (startPos > 0) {
          data += processedText.substring(startPos, pos).trim
          startPos = pos+1
        } else {
          name = processedText.substring(2, pos).trim
          startPos = pos+1
        }
      }

      pos += 1
    }

    new Template(name, data)

//    val pattern = """\s*(\w+)\s+([\w\s]+)""".r
//    pattern.findFirstMatchIn(name) map  {
//      m => new Template(m.group(1), m.group(2), data)
//    }
  }
}

object WikiParser {

  def links(free: FreeText): List[Link] = free.data.collect{case l: Link => l}
  def render(free: FreeText): String = ""

  def main(args: Array[String]) {
    val infobox = new WikiParser().parseRoyalty(
//      "{{Infobox royalty\n| name = Alexander the Great\n| title = [[Basileus]] of [[Macedon]], [[Hegemony|Hegemon]] of the [[League of Corinth|Hellenic League]], [[Shahanshah]] of [[Persia]], [[Pharaoh]] of [[Ancient Egypt|Egypt]], [[Lord of Asia]]\n| image = Alexander the Great mosaic.jpg\n| caption = small{{Citation | contribution = Alexander fighting king [[Darius III of Persia]] | title = [[Alexander Mosaic]] | publisher = [[Naples National Archaeological Museum]]}}./small\n| succession   = [[List of kings of Macedon|King of Macedonia]]\n| reign = 336–323 BC\n| predecessor  = [[Philip II of Macedon|Philip II]]\n| successor = [[Alexander IV of Macedon|Alexander IV]]br /[[Philip III of Macedon|Philip III]]\n| succession1   = [[Pharaoh|Pharaoh of Egypt]]\n| reign1 = 332–323 BC\n| predecessor1  = [[Darius III]]\n| successor1 = [[Alexander IV of Macedon|Alexander IV]]br /[[Philip III of Macedon|Philip III]]\n| succession2   = [[List of kings of Persia|King of Persia]]\n| reign2 = 330–323 BC\n| predecessor2  = [[Darius III]]\n| successor2 = [[Alexander IV of Macedon|Alexander IV]]br /[[Philip III of Macedon|Philip III]]\n| succession3   = [[Lord of Asia|King of Asia]]\n| reign3 = 331–323 BC\n| predecessor3  = ''New office''\n| successor3 = [[Alexander IV of Macedon|Alexander IV]]br /[[Philip III of Macedon|Philip III]]\n| othertitles =\n| full name = Alexander III of Macedon\n| native_lang1 = [[Greek language|Greek]]\n| native_lang1_name1 ={{plainlist |\n* Μέγας Ἀλέξανδρος{{Cref2|d}} (''Mégas Aléxandros'', Great Alexander)\n* Ἀλέξανδρος ὁ Μέγας (''Aléxandros ho Mégas'', Alexander the Great)\n}}\n| spouse = [[Roxana]] of [[Bactria]]br /[[Stateira II]] of [[Persia]]br /[[Parysatis II]] of Persia\n| issue = [[Alexander IV of Macedon|Alexander IV]]\n| house = [[Argead dynasty|Argead]]\n| house-type = Dynasty\n| father = [[Philip II of Macedon]]\n| mother = [[Olympias|Olympias of Epirus]]\n| birth_date = 20 or 21 July 356 BC\n| birth_place = [[Pella]], Macedon\n| death_date = 10 or 11 June 323 BC (aged 32)!-- 32 years, 10 months and 20 days (approx.) --\n| death_place = [[Babylon]]\n| religion = [[Religion in ancient Greece|Greek polytheism]]}}"
//      "{{Infobox royalty\n|name        =Agrippina the Younger\n|image       =Rome Agrippina Minor.jpg\n|caption     =Agrippina, mother of Nero, [[National Museum, Warsaw|National Museum]], [[Warsaw]]\n|succession  =[[List of Roman and Byzantine empresses|Empress consort]] of the [[Roman Empire]]\n|reign       =1 January AD 49 – 13 October AD 54\n|reign-type  =Tenure\n|spouse      =[[Gnaeus Domitius Ahenobarbus (consul 32)|Gnaeus Domitius Ahenobarbus]]&lt;br /&gt;[[Gaius Sallustius Crispus Passienus]]&lt;br&gt;[[Claudius]]\n|issue       =[[Nero|Nero, Emperor of Rome]]\n|house       =[[Julio-Claudian dynasty|Julio-Claudian Dynasty]]\n|father      =[[Germanicus]]\n|mother      =[[Agrippina the Elder]]\n|birth_date  =7 November AD 15\n|birth_place =[[Cologne|Oppidum Ubiorum]] ([[Cologne]])\n|death_date  =23 March AD 59 (aged 43)\n|death_place =[[Misenum]]\n|place of burial =[[Misenum]]\n|}}"
//      "{{Infobox royalty|monarch\n| name            = Alfonso XII\n| succession      = [[King of Spain]] ''([[List of titles and honours of the Spanish Crown|more]])''\n| image           = King Alfonso XII.jpg\n| caption         = 1884 photograph\n| reign           = 29 December 1874 – &lt;br&gt; 25 November 1885\n| coronation      = \n| predecessor     = [[Amadeo I of Spain|Amadeo I]] &lt;br&gt; {{small|''as King of Spain''}} &lt;br&gt; [[Francisco Serrano, 1st Duke of la Torre|Francisco Serrano]] &lt;br&gt; {{small|''as President of the Republic''}}\n| successor       = [[Alfonso XIII of Spain|Alfonso XIII]]\n| reg-type        = {{nowrap|[[Prime Minister of Spain|Prime Ministers]]}}\n| regent          = {{List collapsed|title=''See list''|1=[[Antonio Cánovas del Castillo]]&lt;br&gt;[[Joaquín Jovellar y Soler]]&lt;br&gt;[[Arsenio Martínez Campos]]&lt;br&gt;[[Práxedes Mateo Sagasta]]&lt;br&gt;[[José Posada Herrera]]}}\n| spouse          = [[Mercedes of Orléans]]&lt;br/&gt;[[Maria Christina of Austria]]\n| issue           = [[Mercedes, Princess of Asturias]]&lt;br/&gt;[[Infanta Maria Teresa of Spain|Maria Teresa, Princess Ferdinand of Bavaria]]&lt;br/&gt;[[Alfonso XIII of Spain|Alfonso XIII]]\n| issue-link = #Second marriage and rule\n| issue-pipe = more...\n| house       = [[House of Bourbon|Bourbon]]\n| father          = [[Francis of Assisi de Bourbon]]\n| mother          = [[Isabella II of Spain]]\n| birth_date      = {{Birth date|1857|11|28|df=y}}\n| birth_place     = [[Madrid]], [[Mid-nineteenth century Spain|Spain]]\n| death_date      = {{Death date and age|1885|11|25|1857|11|28|df=y}}\n| death_place     = [[Madrid]], [[Restoration (Spain)|Spain]]\n| place of burial = [[El Escorial]]\n| religion        = [[Catholic Church|Roman Catholic]]\n}}"
      "{{infobox royalty\n| name         = Alfonso III\n| image        = Jaume Mateu - Alfons III the Liberal - Google Art Project.jpg\n| succession   = [[King of Aragon]], [[King of Valencia|Valencia]] and [[Count of Barcelona]] \n| reign        = 1285–1291\n| coronation   = 2 February 1286 (Valencia)&lt;br /&gt;9 April 1286 (Zaragoza)\n| predecessor  = [[Peter III of Aragon|Peter III]]\n| successor     = [[James II of Aragon|James II]]\n| spouse       = \n| issue        = \n| house        = [[House of Barcelona]]\n| father       = [[Peter III of Aragon]]\n| mother       = [[Constance of Sicily, Queen of Aragon|Constance of Sicily]]\n| birth_date   = 4 November 1265\n| birth_place  = [[Valencia]]\n| death_date   = 18 June 1291 (aged 26)\n| death_place  = [[Barcelona]]\n| burial_date  =\n| burial_place = [[Barcelona Cathedral]]; prev. Convent de San Francisco, Barcelona\n| religion     = [[Catholic Church|Roman Catholicism]]\n}}"
//      "{{infobox royalty\n| name         = Alfonso IV\n| image        = Alifonso IV d'Aragón.jpg\n| succession   = [[King of Aragon]], [[King of Valencia|Valencia]], [[King of Sicily|Sicily]], [[King of Sardinia and Corsica|Sardinia and Corsica]], [[Count of Barcelona]] \n| reign        = 1327–1336\n| predecessor  = [[James II of Aragon|James II]]\n| successor     = [[Peter IV of Aragon|Peter IV]]\n| spouse       = [[Teresa d'Entença]]&lt;br/&gt;[[Eleanor of Castile (1307-1359)|Eleanor of Castile]]\n| issue        = Alfonso&lt;br /&gt;[[Constance of Aragon, Queen of Majorca|Constance, Queen of Majorca]]&lt;br /&gt;[[Peter IV of Aragon|Peter IV, King of Aragon]]&lt;br /&gt;[[James I of Urgell|James I, Count of Urgell]]\n| house        = [[House of Barcelona]]\n| father       = [[James II of Aragon]]\n| mother       = [[Blanche of Anjou]]\n| birth_date   = 2 November 1299\n| birth_place  = [[Naples]]\n| death_date   = 24 January 1336\n| death_place  = [[Barcelona]]\n}}"
//      "{{Infobox royalty\n| type         = monarch\n| name         = Alfonso the Magnanimous\n| image        = Alfonso-V-el-Magnanimo.jpg\n| caption      = Portrait of Alfonso V of Aragon, by 16th century painter [[Vicente Juan Masip]]\n| spouse       = [[Maria of Castile]]\n| succession   = [[Crown of Aragon|King of Aragon]]\n| reign        = 2 April 1416 – 27 June 1458\n| predecessor  = [[Ferdinand I of Aragon|Ferdinand I]]\n| successor    = [[John II of Aragon|John II]]\n| succession1  = [[List of monarchs of Naples|King of Naples]] [[List of monarchs of Sicily|and Sicily]]\n| reign1       = 2 June 1442 – 27 June 1458\n| predecessor1 = [[René of Anjou|René]]\n| successor1   = [[Ferdinand I of Naples|Ferdinand I]]\n| issue        = [[Ferdinand I of Naples]] &lt;small&gt;(illegitimate)&lt;/small&gt;\n| issue-link   = #Family\n| issue-pipe   = among others...\n| house        = [[House of Trastámara]]\n| father       = [[Ferdinand I of Aragon]]\n| mother       = [[Eleanor of Alburquerque]]\n| birth_date   = 1396\n| birth_place  = [[Medina del Campo]], [[Kingdom of Castile]]\n| death_date   = 27 June {{death year and age|df=yes|1458|1396}}\n| death_place  = [[Castel dell'Ovo]], [[Naples]], [[Kingdom of Naples]]\n| burial_date  = \n| burial_place = [[Poblet Monastery]]\n| religion     = [[Catholic Church|Roman Catholicism]]\n}}"

    )

//    println(s"name: ${infobox.get.name}")
    infobox.map(_.job(0).succession.raw).foreach(println(_))
    infobox.map(_.job(0).succession.parsed).foreach(println(_))
    infobox.map(_.job(0).succession.parsed).map(links(_)).foreach(println(_))
//    infobox.get.data.foreach(println(_))
  }
}
