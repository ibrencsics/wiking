package xml

import java.io.{FileOutputStream, File}

import parser.WikiParser

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.xml.XML
import scala.xml.pull.XMLEventReader

object RoyaltyFilter extends App {

  val inputFile = "/home/ivan/opt/wikidump/enwiki/infobox-filtered.xml"
  val inputXml = new XMLEventReader(Source.fromFile(inputFile))

  val outputFile = new File("/home/ivan/opt/wikidump/enwiki/", "royalty-filtered.xml")
  val out = new FileOutputStream(outputFile)
  println("Writing to: " + outputFile.getAbsolutePath())

  new XmlParser().parse(inputXml, filter)
  out.close()

  def filter(buf: ArrayBuffer[String]): Unit = {
    try {
      val s = buf.mkString
      val x = XML.loadString(s)

      val infoboxType = (x \ "type") (0).child(0).toString.toLowerCase.trim

      if (infoboxType == "royalty") {
        val name = (x \ "name") (0).child(0).toString.trim
        val text = (x \ "text") (0).child(0).toString()

        val royalty = new parser.WikiParser().parseRoyalty(text)

        out.write(s"${name}\n".getBytes())
        out.write(royalty
//          .map(_.succession.map(_.raw).mkString("\t", "\n\t", "\n"))
          .map(_.job(0).succession.parsed).map(WikiParser.links(_)).mkString("\t", "\n\t", "\n")
//          .getOrElse("-\n")
          .getBytes())
//        royalty.flatMap(_.title).foreach(s => out.write(s"\n\ttitle : ${s}\n".getBytes()))
      }

    } catch {
      case e: Exception =>
    }
  }
}