package xml

import java.io.{FileOutputStream, File}
import java.util.Date

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.xml.XML
import scala.xml.pull.XMLEventReader

object RedirectFilter extends App {

  val inputFile = "/home/ivan/opt/wikidump/enwiki/enwiki-latest-pages-articles.xml"
//  val inputFile = "/home/ivan/opt/wikidump/enwiki/redirect.xml"
  val inputXml = new XMLEventReader(Source.fromFile(inputFile))

  val outputFile = new File("/home/ivan/opt/wikidump/enwiki/", "redirects.csv")
  val out = new FileOutputStream(outputFile)

  val pre = new Date()
  new XmlParser().parse(inputXml, writePage)
  val post = new Date()

  println(s"Job finished in ${post.getTime - pre.getTime} ms.")


  def writePage(buf: ArrayBuffer[String]) = {
    var pageTitle: String = null

    try {

      val s = buf.mkString
      val x = XML.loadString(s)

      pageTitle = (x \ "title") (0).child(0).toString
      val redirect = (x \ "redirect" \ "@title")

      if (redirect != null && redirect.nonEmpty) {
        out.write(s"${pageTitle}; ${redirect}\n".getBytes)
      }

    } catch {
      case e: Exception => /*println(s"Failed to parse article '${pageTitle}': ${e.getMessage}")*/
    }
  }
}
