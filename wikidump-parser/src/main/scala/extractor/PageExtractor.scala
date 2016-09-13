package extractor

import java.util.regex.Pattern

import scala.annotation.tailrec

object PageExtractor {

  val infoboxRegexp = "\\{\\{[iI]nfobox\\s((?:(?![\\n|&\\}!]).)*)"
  val infoboxPattern = Pattern.compile(infoboxRegexp)

  def getInfobox(pageText: String): Option[String] = {

    val matcher = infoboxPattern.matcher(pageText)
    while (matcher.find()) {

      val startPos: Int = matcher.start()
      val endPos: Int = infoboxEnd(pageText, startPos)

      Some(pageText.substring(startPos, endPos))
    }

    def infoboxEnd(page: String, startPos: Int): Int = {

      @tailrec
      def loop(pos: Int, sum: Int): Int = {

        val pChar = page.charAt(pos)

        if (pChar equals '{')
          loop(pos + 1, sum + 1)

        else if (pChar equals '}')
          loop(pos + 1, sum - 1)

        else if (sum == 0)
          pos

        else
          loop(pos + 1, sum)
      }

      loop(startPos, 0)
    }

    None
  }
}