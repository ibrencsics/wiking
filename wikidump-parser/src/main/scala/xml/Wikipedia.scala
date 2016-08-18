package xml

import java.util.Date

import scala.annotation.tailrec
import scala.io.Source
import scala.xml.pull._
import scala.collection.mutable.ArrayBuffer
import java.io.File
import java.io.FileOutputStream
import scala.xml.XML

import java.util.regex.Pattern;


object Wikipedia extends App {

//  val inputFile = "/home/ivan/opt/wikidump/enwiki/enwiki-head-1M.xml"
  val inputFile = "/home/ivan/opt/wikidump/enwiki/enwiki-latest-pages-articles.xml"
  val inputXml = new XMLEventReader(Source.fromFile(inputFile))

  val outputFile = new File("/home/ivan/opt/wikidump/enwiki/", "infobox.xml")
  val out = new FileOutputStream(outputFile)
  println("Writing to: " + outputFile.getAbsolutePath())

  val infoboxRegexp = "\\{\\{[iI]nfobox\\s((?:(?![\\n|&\\}!]).)*)"
  val infoboxPattern = Pattern.compile(infoboxRegexp)


  out.write("<infoboxes>\n".getBytes())
  val pre = new Date()

  new XmlParser().parse(inputXml, writePage)

  val post = new Date()
  out.write("</infoboxes>".getBytes)
  out.close()

  println(s"Job finished in ${post.getTime - pre.getTime} ms.")


  def writePage(buf: ArrayBuffer[String]) = {
    var pageTitle: String = null

    try {

      val s = buf.mkString
      val x = XML.loadString(s)

      pageTitle = (x \ "title")(0).child(0).toString
      val pageText = (x \ "revision" \ "text")(0).child(0).toString

      val matcher = infoboxPattern.matcher(pageText)
      while (matcher.find()) {

        val startPos: Int = matcher.start()
        val endPos: Int = infoboxEnd(pageText, startPos)

        out.write(createXmlItem(pageTitle, matcher.group(1).trim, pageText.substring(startPos, endPos)).getBytes)
      }
    } catch {
      case e: Exception => println(s"Failed to parse article '${pageTitle}': ${e.getMessage}")
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

    def createXmlItem(title: String, infoboxType: String, infobox: String): String = {
      val item =
      "\t<page>\n" +
      "\t\t<name>" + title + "</name>\n" +
      "\t\t<type>" + infoboxType + "</type>\n" +
      "\t\t<text>\n" +
      infobox + "\n" +
      "\t\t</text>\n" +
      "\t</page>\n"

      item
    }
  }
}