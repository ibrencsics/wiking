package xml

import java.io.{FileInputStream, FileOutputStream, File}

import scala.collection.mutable.{ListBuffer, ArrayBuffer}
import scala.io.Source
import scala.xml.XML
import scala.xml.pull.XMLEventReader

object InfoboxFilter extends App {

//  val inputFile = "/home/ivan/opt/wikidump/enwiki/infobox-20160616.xml"
  val inputFile = "/home/ivan/opt/wikidump/enwiki/infobox-20160701.xml"
  val inputXml = new XMLEventReader(Source.fromFile(inputFile))

  val filterFile = "/home/ivan/opt/wikidump/enwiki/infobox-statistics-important-20160616.xml"

  val outputFile = new File("/home/ivan/opt/wikidump/enwiki/", "infobox-filtered.xml")
  val out = new FileOutputStream(outputFile)
  println("Writing to: " + outputFile.getAbsolutePath())

  val importants = loadImportants()

  println(importants)

  out.write("<infoboxes>\n".getBytes())

  new XmlParser().parse(inputXml, filter)

  out.write("</infoboxes>".getBytes)
  out.close()


  def filter(buf: ArrayBuffer[String]): Unit = {

    try {
      val s = buf.mkString
      val x = XML.loadString(s)

      val name = (x \ "name") (0).child(0).toString.toLowerCase.trim
      val infoboxType = (x \ "type") (0).child(0).toString.toLowerCase.trim

      if (importants.contains(infoboxType)) {
        out.write(s.getBytes)
      }

    } catch {
      case e: Exception =>
    }
  }

  def loadImportants(): List[String] = {
    val filters = ListBuffer[String]()

    for (line <- Source.fromFile(filterFile).getLines()) {
      filters += line.split(":")(0).trim
    }

    filters.toList
  }
}
