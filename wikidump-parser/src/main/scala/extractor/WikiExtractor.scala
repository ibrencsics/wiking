package extractor

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.matching.Regex


object Line {
  def unapply(raw: String): Option[(String, String)] = {
    val index = raw.indexOf('=')
    if (index >= 0)
      Some(raw.substring(0, index).trim, raw.substring(index + 1).trim)
    else
      None
  }
}

//case class Template(name: String, data: List[String])
case class ParsedTemplate(name: String, data: List[(String, List[Element])])

sealed trait Element
case class Link(page: String, plain: Option[String]) extends Element
case class Template(data: String) extends Element
case class OfCombined(name: Element, country: Element) extends Element
case class Text(plain: String) extends Element
case class Sep(data: String) extends Element
case class Of(plain: String) extends Element
case class Lf(plain: String) extends Element
case class Date(year: String, month: String, day: String, ad: Boolean) extends Element
case class Timeframe(from: Date, to: Date) extends Element


class FreeText(/*data: List[Element]*/)

object FreeText {

  type Free = List[Element]

  val REGEXP_SEP = """^\s*(,)$|^(and)\s*$""".r
  val REGEXP_OF = """^of\s*(the)*$""".r
  val REGEXP_LF = """(&lt;br\s*/{0,1}&gt;)|(\\n)|(&lt;!--.*--&gt;)""".r // the last of is comment
  val REGEXP_SMALL = """&lt;/?small\s*/{0,1}&gt;""".r

  val R_YEAR = "(\\d{1,4})"
  val R_MONTH = "([Jj]anuary|[Ff]ebruary|[Mm]arch|[Aa]pril|[Mm]ay|[Jj]une|[Jj]uly|[Aa]ugust|[Ss]eptember|[Oo]ctober|[Nn]ovember|[Dd]ecember)"
  val R_DAY = "(\\d{1,2}|\\d{1,2}\\sor\\s\\d{1,2})" //\\s+(or\\s+\\d{1,2}))"
  val R_AD_BC = "(AD|BC)?"
  val R_DATE = new Regex(R_DAY + "\\s" + R_MONTH + "\\s" + R_YEAR + "\\s?" + R_AD_BC)
  val R_FROM_TO = new Regex(R_DATE.regex + "\\s*[–-]\\s*" + R_DATE.regex)

  val R_FROM_TO_YEARS_ONLY = new Regex(R_YEAR + "\\s?" + "–" + R_YEAR + "\\s?" + R_AD_BC)

  // http://stackoverflow.com/questions/15491894/regex-to-validate-date-format-dd-mm-yyyy

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

      } else if (R_FROM_TO.findFirstIn(trimmed).nonEmpty) {
        R_FROM_TO.findAllIn(trimmed).matchData foreach {
          m => elems += Timeframe(
            Date(m.group(1), m.group(2), m.group(3), isAd(m.group(4))),
            Date(m.group(5), m.group(6), m.group(7), isAd(m.group(8))))
        }

      } else if (R_DATE.findFirstIn(trimmed).nonEmpty) {
        R_DATE.findAllIn(trimmed).matchData foreach {
          m => elems += Date(m.group(1), m.group(2), m.group(3), isAd(m.group(4)))
        }

      } else if (R_FROM_TO_YEARS_ONLY.findFirstIn(trimmed).nonEmpty) {
        R_FROM_TO_YEARS_ONLY.findAllIn(trimmed).matchData foreach {
          m => elems += Timeframe(Date("", "", m.group(1), isAd(m.group(3))), Date("", "", m.group(2), isAd(m.group(3))))
        }

//      } else if (new Regex(R_YEAR).findFirstIn(trimmed).nonEmpty) {
//        new Regex(R_YEAR).findAllIn(trimmed).matchData foreach {
//          m => elems += Date("", "", m.group(1), true)
//        }

      } else if (trimmed.size > 0 || trimmed == ' ') {
        elems +=  Text(trimmed)
      }

      def isAd(data: String): Boolean = data == null || (data != null && data == "AD")
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
        if (buf.nonEmpty) freeElement()
        linkLevel += 1
      }
      else if (c equals ']') {
        linkLevel -= 1
      }
      else if (c equals '{') {
        if (buf.nonEmpty) freeElement()
        templateLevel += 1
      }
      else if (c equals '}') {
        templateLevel -= 1
      }

      if (!linkPresent && linkLevel == 2) {
        linkPresent = true
        linkStart = pos + 1
      }
      else if (linkPresent && linkLevel == 0) {
        val link = raw.substring(linkStart, pos - 1)
        val split = link.split("\\|")
        elems += Link(split(0), if (split.size > 1) Option(split(1)) else Option.empty)
        linkPresent = false
      }
      else if (!templatePresent && templateLevel == 2) {
        templatePresent = true
        templateStart = pos + 1
      }
      else if (templatePresent && templateLevel == 0) {
        val template = raw.substring(templateStart, pos - 1)
        elems += Template(template)
        templatePresent = false
      }
      else if (!linkPresent && !templatePresent && !c.equals('[') && !c.equals(']') && !c.equals('{') && !c.equals('}')) {
        buf += c.toString
      }




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