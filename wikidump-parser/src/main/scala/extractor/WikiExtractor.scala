package extractor

import scala.collection.mutable.{ArrayBuffer, ListBuffer}


object Line {
  def unapply(raw: String): Option[(String, String)] = {
    val index = raw.indexOf('=')
    if (index >= 0)
      Some(raw.substring(0, index).trim, raw.substring(index + 1).trim)
    else
      None
  }
}

case class Template(name: String, data: List[String])
case class ParsedTemplate(name: String, data: List[(String, List[Element])])

sealed trait Element
case class Link(page: String, plain: Option[String]) extends Element
case class OfCombined(name: Element, country: Element) extends Element
case class Text(plain: String) extends Element
case class Sep(data: String) extends Element
case class Of(plain: String) extends Element
case class Lf(plain: String) extends Element


class FreeText(/*data: List[Element]*/)

object FreeText {

  type Free = List[Element]

  val REGEXP_SEP = """^\s*(,)|(and)\s*$""".r
  val REGEXP_OF = """^of\s*(the)*$""".r
  val REGEXP_LF = """(&lt;br\s*/{0,1}&gt;)|(\\n)|(&lt;!--.*--&gt;)""".r
  val REGEXP_SMALL = """&lt;/?small\s*/{0,1}&gt;""".r

  def unapply(raw: String): Option[List[Element]] = {

    val elems = ListBuffer[Element]()
    var buf = ArrayBuffer[String]()

    def freeElement(): Unit = {
      val text = buf.mkString
      buf.clear()

      // eliminate line feed
      val smallRemoved = REGEXP_SMALL.replaceAllIn(text, " ")
      val trimmed = REGEXP_LF.replaceAllIn(smallRemoved, " ").trim

      if (REGEXP_SEP.findFirstIn(trimmed).nonEmpty) {
        elems += Sep(trimmed)
      } else if (REGEXP_OF.findFirstIn(trimmed).nonEmpty) {
        elems += Of(trimmed)
//      } else if (RegexHelper.Lf.findFirstIn(trimmed).nonEmpty) {
//        elems += Lf(trimmed)
      } else if (trimmed.size > 0 || trimmed == ' ') {
        elems +=  Text(trimmed)
      }
    }


    var pos = 0

    var linkLevel = 0
    var linkStart = -1
    var linkPresent = false

    var templateLevel = 0
    var templateStart = -1
    var templatePresent = false

    for (c: Char <- raw) {
      if (c equals '[') {
//        println("'[' found")

        if (buf.nonEmpty) {
          freeElement()
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

    if (elems.isEmpty) freeElement()
    if (buf.nonEmpty) freeElement()

    Some(elems.result)
  }
}


class WikiExtractor {

  def parseTemplate(text: String): ParsedTemplate = {

    def addToList(freetext: String, list: ListBuffer[(String, List[Element])]): Unit = {
      freetext match {
        case Line(elemName, FreeText(elemValue)) => list += ((elemName, elemValue))
      }
    }

    var level = 0
    var pos = 0
    var startPos = -1

    var name = "unknown"
    val list = new ListBuffer[(String, List[Element])]

    val processedText = text.replaceAll("\\n", "")

    for (c: Char <- processedText) {
      if (c == '{' || c == '[') level += 1
      else if (c == '}' || c == ']') level -= 1

      if (c == '|' && level == 2) {
        if (startPos > 0) {
          addToList(processedText.substring(startPos, pos).trim, list)
          startPos = pos + 1
        } else {
          name = processedText.substring(2, pos).trim
          startPos = pos + 1
        }
      }

      pos += 1
    }

    addToList(processedText.substring(startPos, pos - 2).trim, list)

    new ParsedTemplate(name, list.result)
  }
}

object WikiExtractor {
  def getNum(r: scala.util.matching.Regex, raw: String): Option[Int] = {
    r.findFirstMatchIn(raw).map(_.group(1)).map(i => if (i.isEmpty) 0 else Integer.valueOf(i))
  }
}


// succession -> 0  : [[Crown of Aragon|King of Aragon]]
// reign -> 0       : 2 April 1416 – 27 June 1458
// predecessor -> 0 : [[Ferdinand I of Aragon|Ferdinand I]]
// successor -> 0   : [[John II of Aragon|John II]]
// succession -> 1  : [[List of monarchs of Naples|King of Naples]] [[List of monarchs of Sicily|and Sicily]]
// reign -> 1       : 2 June 1442 – 27 June 1458
// predecessor -> 1 : [[René of Anjou|René]]
// successor -> 1   : [[Ferdinand I of Naples|Ferdinand I]]


// TODO
// Link(Basileus,None), Of(of), Link(Macedon,None)
// Text(, Macedon)
// deathDate -> Text(27 June {{death year and age|df=yes|1458|1396}})
// Text(&lt;small&gt;(illegitimate)&lt;/small&gt;)