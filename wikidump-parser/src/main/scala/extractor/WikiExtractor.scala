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
case class Circa(date: Element) extends Element


class FreeText(/*data: List[Element]*/)

object FreeText {

  type Free = List[Element]

  val R_LINK = """^\s*\[\[([^\[\]\|]+)\|?([^\[\]\|]*)\]\]\s*$""".r
  val R_TEMPLATE = """^\s*\{\{(.*)\}\}\s*$""".r

  val R_SEP = """(^\s*,\s*$|^\s*and\s*$)""".r
  val R_OF = """(^of\s*(the)*$)""".r

  val R_LF = """(&lt;br\s*/{0,1}&gt;)|(\\n)""".r
  val R_SMALL = """&lt;/?small\s*/{0,1}&gt;""".r
  val R_MISC = """(&lt;!--.*--&gt;)|(&amp;)|(nbsp;)|(''\(uncertain\)'')""".r

  val R_YEAR = "(\\d{1,4})"
  val R_MONTH = "([Jj]anuary|[Ff]ebruary|[Mm]arch|[Aa]pril|[Mm]ay|[Jj]une|[Jj]uly|[Aa]ugust|[Ss]eptember|[Oo]ctober|[Nn]ovember|[Dd]ecember)"
  val R_DAY = "(\\d{1,2}|\\d{1,2}\\sor\\s\\d{1,2})"
  val R_AD_BC = "(AD|BC|CE|BCE)"

  // 11 September 1771
  val R_DATE_1 = new Regex(R_DAY + "\\s*" + R_MONTH + "\\s*" + R_YEAR + "\\s?" + R_AD_BC + "?.*")
  // May 20, 1851
  val R_DATE_2 = new Regex(R_MONTH + "\\s*" + R_DAY + ",\\s*" + R_YEAR + ".*")
  // July 4, 1873
  val R_DATE_4 = new Regex(R_MONTH + "\\s*" + R_DAY + ",\\s*" + R_YEAR + ".*")
  // 50 BC
  val R_DATE_5 = new Regex("\\s*" + R_YEAR + "s*" + "\\s*" + R_AD_BC + "?")
  // 50 -
  val R_DATE_5_DASH = new Regex("\\s*" + R_YEAR + "s*" + "\\s*" + R_AD_BC + "?\\s*–")
  // November 1204
  val R_DATE_6 = new Regex(R_MONTH + "\\s*" + R_YEAR + "\\s*?" + R_AD_BC + "?")
  // 1 August AD 12
  val R_DATE_7 = new Regex(R_DAY + "\\s*" + R_MONTH + "\\s*" + R_AD_BC + "\\s*" + R_YEAR + ".*")

  val R_CIRCA = """(.*)\bcirca\b(.*)""".r

  val R_FROM_TO = new Regex(R_DATE_1.regex + "\\s*[–-]\\s*" + R_DATE_1.regex + ".*")
  val R_FROM_TO_AROUND_ZERO = new Regex(R_DATE_1.regex + "\\s*[–-]\\s*" + R_DATE_7.regex + ".*")
  val R_FROM_TO_YEARS_ONLY = new Regex(R_YEAR + "\\s?" + "[–/]" + R_YEAR + "\\s?" + R_AD_BC + "?.*")

  val R_T_BIRTH_DATE = """[Bb]irth[\s_]date(\sand\sage)?\s*\|(.*\|)?(\d{1,4})\|(\d{1,2})\|(\d{1,2}).*""".r
  val R_T_BIRTH_DASH_DATE = """Birth-date\|([^\|]*)\|?(.*)?""".r
  val R_T_BIRTH_DATE_AND_AGE = """BirthDeathAge\|(B|\s*)\|(\d{1,4})\|(\d{1,2}|\s*)\|(\d{1,2}|\s*)\|(\d{1,4})\|?(\d{1,2}|\s)?\|?(\d{1,2}|\s)?\|?.*""".r

  val R_T_DEATH_DATE = """[Dd]eath\sdate\s*\|(.*\|)?(\d{1,4})\|(\d{1,2})\|(\d{1,2}).*""".r
  val R_T_DEATH_DATE_AND_AGE = """[Dd]eath\sdate(\sand\sage)?\s*\|(.*\|)?(\d{1,4})\|(\d{1,2})\|(\d{1,2})\|(\d{1,4})\|(\d{1,2})\|(\d{1,2}).*""".r
  val R_T_DEATH_YEAR = """[Dd]eath\syear(\sand\sage)?\s*\|(.*\|)?(\d{1,4})\|(\d{1,4}).*""".r
  val R_T_DEATH_DASH_DATE = """[Dd]eath-date(\sand\sage)?\s*\|([^\|]*)\|?(.*)?""".r
  val R_T_NOWRAP = """nowrap\s*\|(.*)""".r
  val R_T_CIRCA = """circa""".r
  val R_T_CIRCA_2 = """circa\|([^\|]+)""".r
  val R_T_CIRCA_3 = new Regex("circa\\|" + R_YEAR + "\\|" + R_YEAR + "\\s*" + R_AD_BC + "?.*")
  val R_T_CIRCA_4 = """circa\|\{\{([^\{\}]+)\}\}""".r

  // http://stackoverflow.com/questions/15491894/regex-to-validate-date-format-dd-mm-yyyy


  def unapply(raw: String): Option[List[Element]] = {

    val elems = ListBuffer[Element]()
    var buf = ArrayBuffer[String]()


    def addBuffer(): Unit = {
      def free(): String = {
        val text = buf.mkString
        buf.clear()

        text
          .replaceAll(R_SMALL.regex, " ")
          .replaceAll(R_LF.regex, " ")
          .replaceAll(R_MISC.regex, " ")
          .trim
      }

      parse(free()).foreach(elems += _)
    }

    def add(s: String): Unit = {
      parse(s).foreach(elems += _)
    }

    def parse(text: String): Option[Element] = text match {
      case R_LINK(name, page) if page.nonEmpty => Option(Link(name, Option(page)))
      case R_LINK(name, page) => Option(Link(name, Option.empty))
      case R_TEMPLATE(t) => parseTemplate(t)
      case t: String => parseDate(t).orElse(parseMisc(t))
    }

    def parseTemplate(text: String): Option[Element] = text match {
      case R_T_BIRTH_DATE(_, _, y, m, d) => Option(Date(d, m, y, true))
      case R_T_DEATH_DATE(_, y, m, d) => Option(Date(d, m, y, true))
      case R_T_DEATH_DATE_AND_AGE(_, _, y, m, d, _, _, _) => Option(Date(d, m, y, true))
      case R_T_DEATH_YEAR(_, _, dy, by) => Option(Date("", "", dy, true))
      case R_T_BIRTH_DATE_AND_AGE(_, by, bm, bd, dy, dm, dd) => Option(
        Timeframe(Date(spaceToEmpty(bd), spaceToEmpty(bm), by, true), Date(spaceToEmpty(dd), spaceToEmpty(dm), dy, true)))
      case R_T_BIRTH_DASH_DATE(d1, d2) => parseDate(if (d2==null || d2=="") d1 else d2)
      case R_T_DEATH_DASH_DATE(_, d1, d2) => parseDate(if (d2==null || d2=="") d1 else d2)
      case R_T_NOWRAP(t) => parse(t)
      case R_T_CIRCA_3(y1, y2, a) => Option(Circa(Timeframe(Date("","",y1,isAd(a)), Date("","",y2,isAd(a)))))
      case R_T_CIRCA_4(y) => parseTemplate(y)
      case R_T_CIRCA_2(y) => parseDate(y).map(Circa).orElse(Option(Circa(null)))
      case R_T_CIRCA() => Option(Circa(null))
      case t => Option(Template(t))
    }

    def parseDate(raw: String): Option[Element] = raw match {
      case R_FROM_TO(d1, m1, y1, a1, d2, m2, y2, a2) => Option(Timeframe(Date(d1, m1, y1, isAd(a1)), Date(d2, m2, y2, isAd(a2))))
      case R_FROM_TO_AROUND_ZERO(d1, m1, y1, a1, d2, m2, a2, y2) => Option(Timeframe(Date(d1, m1, y1, isAd(a1)), Date(d2, m2, y2, isAd(a2))))
      case R_FROM_TO_YEARS_ONLY(y1, y2, a) => Option(Timeframe(Date("", "", y1, isAd(a)), Date("", "", y2, isAd(a))))

      case R_DATE_1(d, m, y, a) => Option(Date(d, m, y, isAd(a)))
      case R_DATE_2(m, d, y) => Option(Date(d, m, y, true))
      case R_DATE_4(m, d, y) => Option(Date(d, m, y, true))
      case R_DATE_5_DASH(y, a) => Option(Date("", "", y, isAd(a)))
      case R_DATE_5(y, a) => Option(Date("", "", y, isAd(a)))
      case R_DATE_6(m, y, a) => Option(Date("", m, y, isAd(a)))
      case R_DATE_7(d, m, a, y) => Option(Date(d, m, y, isAd(a)))

      case R_CIRCA(pre, post) => parseDate(pre.trim + post.trim).map(Circa)

      case _ => Option.empty
    }

    def parseMisc(raw: String): Option[Element] = raw match {
      case R_SEP(text) => Option(Sep(text))
      case R_OF(text, _) => Option(Of(text))
      case text if text.nonEmpty => Option(Text(text))
      case _ => Option.empty
    }

    def spaceToEmpty(t: String): String = if (t == null || t == " ") "" else t

    def isAd(data: String): Boolean = data == null || (data != null && (data == "AD" || data == "CE"))



    var pos = 0

    var linkLevel = 0
    var linkStart = -1
    var linkPresent = false

    var templateLevel = 0
    var templateStart = -1
    var templatePresent = false

    for (c: Char <- raw) {
      if (c equals '[') {
        if (buf.nonEmpty) addBuffer()
        linkLevel += 1
      }
      else if (c equals ']') {
        linkLevel -= 1
      }
      else if (c equals '{') {
        if (buf.nonEmpty) addBuffer()
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
        add(s"[[${link}]]")
        linkPresent = false
      }
      else if (!templatePresent && templateLevel == 2) {
        templatePresent = true
        templateStart = pos + 1
      }
      else if (templatePresent && templateLevel == 0) {
        val template = raw.substring(templateStart, pos - 1)
        add(s"{{${template}}}")
        templatePresent = false
      }
      else if (!linkPresent && !templatePresent && !c.equals('[') && !c.equals(']') && !c.equals('{') && !c.equals('}')) {
        buf += c.toString
      }

      pos += 1
    }

    if (elems.isEmpty) addBuffer()
    if (buf.nonEmpty) addBuffer()

    Some(elems.result)
  }
}


class WikiExtractor {

  def addToList(freetext: String, list: ListBuffer[(String, List[Element])]): Unit = {
    parseLine(freetext).foreach(list += _)
  }

  def parseLine(freetext: String): Option[(String, List[Element])] = freetext match {
    case Line(elemName, FreeText(elemValue)) => Option((elemName, postProcess(elemValue)))
//    case Line(elemName, FreeText(elemValue)) => Option((elemName, elemValue))
    case t => Option.empty
  }

  def postProcess(toBePostProcessed: List[Element]): List[Element] = {
    toBePostProcessed
      .foldLeft(List[Element]())((l,r) =>
        if (l.size > 0 && l.last == Link("circa",Some("c.")) && (r.isInstanceOf[Date] || r.isInstanceOf[Timeframe])) l :+ Circa(r) else l :+ r)
      .filter(_ != Link("circa",Some("c.")))
  }

  def parseTemplate(text: String): ParsedTemplate = {
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
