package xml

import java.io.{FileOutputStream, File}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.xml.XML
import scala.xml.pull.XMLEventReader

object InfoboxParser extends App {

  val inputFile = "/home/ivan/opt/wikidump/enwiki/infobox-20160616.xml"
  val inputXml = new XMLEventReader(Source.fromFile(inputFile))

  val outputFile = new File("/home/ivan/opt/wikidump/enwiki/", "infobox-statistics.xml")
  val out = new FileOutputStream(outputFile)
  println("Writing to: " + outputFile.getAbsolutePath())


  val result = scala.collection.mutable.Map[String, Integer]()

  new XmlParser().parse(inputXml, count)

  storeResult()

  def count(buf: ArrayBuffer[String]): Unit = {

    try {

      val s = buf.mkString
      val x = XML.loadString(s)

      val name = (x \ "name") (0).child(0).toString.toLowerCase.trim
      val infoboxType = (x \ "type") (0).child(0).toString.toLowerCase.trim

      if (!result.contains(infoboxType)) {
        result.put(infoboxType, 1)
      } else {
        result.put(infoboxType, result.get(infoboxType).get + 1)
      }
    } catch {
      case e: Exception =>
    }

//    result.put(infoboxType, result.get(infoboxType).map(_ + 1) getOrElse 1)
  }

  def storeResult(): Unit = {
    val s = result.map{ case (k,v) => s"${k} : ${v}" }.mkString ("", "\n", "\n")
    out.write(s.getBytes)
    out.close
  }
}
