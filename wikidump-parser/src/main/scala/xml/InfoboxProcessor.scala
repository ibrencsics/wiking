package xml

import java.io.{FileOutputStream, File}
import java.util.Date

import extractor.{RoyaltyExtractor, Royalty}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.xml.XML
import scala.xml.pull.XMLEventReader

object InfoboxProcessor extends App {

  val inputFile = "/home/ivan/opt/wikidump/enwiki/infobox-filtered.xml"
  val inputXml = new XMLEventReader(Source.fromFile(inputFile))

  val outputFile = new File("/home/ivan/opt/wikidump/enwiki/", "infobox-parsed.xml")
  val out = new FileOutputStream(outputFile)
  println("Writing to: " + outputFile.getAbsolutePath())

  var i = 0;
  var j = 0


  out.write("<infoboxes>\n".getBytes())
  val pre = new Date()

  new XmlParser().parse(inputXml, writePage)

  val post = new Date()
  out.write("</infoboxes>".getBytes)
  out.close()

  println(s"Job finished in ${post.getTime - pre.getTime} ms.")


  def writePage(buf: ArrayBuffer[String]): Unit = {

    var pageTitle: String = null

    try {

      val s = buf.mkString
      val x = XML.loadString(s)

      val pageType = (x \ "type")(0).child(0).toString

      if (pageType == "royalty") {
        j += 1

        pageTitle = (x \ "name") (0).child(0).toString
        val pageText = (x \ "text" ) (0).child(0).toString

        out.write(createXmlItem(pageTitle, pageType, pageText, new RoyaltyExtractor().extract(pageText)).getBytes)

        i += 1;
        if (i % 100 == 0) println(s"${i} ${j}")
//        if (i > 100) System.exit(0)
      }


    } catch {
      case e: Exception => println(pageTitle)
    }


      def createXmlItem(title: String, infoboxType: String, text: String, royalty: Royalty): String = {
      val item =
        "\t<page>\n" +
          "\t\t<name>" + title + "</name>\n" +
          "\t\t<type>" + infoboxType + "</type>\n" +
          "\t\t<text>\n" +
          text + "\n" +
          "\t\t</text>\n" +
          "\t\t<data>\n" +
          "\t\t\t<name>" + royalty.name + "</name>\n" +
          "\t\t\t<birthDate>" + royalty.birthDate + "</birthDate>\n" +
          "\t\t\t<birthPlace>" + royalty.birthPlace + "</birthPlace>\n" +
          "\t\t\t<deathDate>" + royalty.deathDate + "</deathDate>\n" +
          "\t\t\t<deathPlace>" + royalty.deathPlace + "</deathPlace>\n" +
          "\t\t\t<spouse>" + royalty.spouse + "</spouse>\n" +
          "\t\t\t<issue>" + royalty.issue + "</issue>\n" +
          "\t\t\t<father>" + royalty.father + "</father>\n" +
          "\t\t\t<mother>" + royalty.mother + "</mother>\n" +
          "\t\t\t<house>" + royalty.house + "</house>\n" +
          "\t\t\t<religion>" + royalty.religion + "</religion>\n" +
          "\t\t\t<succession>" + royalty.succession + "</succession>\n" +
          "\t\t\t<predecessor>" + royalty.predecessor + "</predecessor>\n" +
          "\t\t\t<successor>" + royalty.successor + "</successor>\n" +
          "\t\t\t<reign>" + royalty.reign + "</reign>\n" +
          "\t\t</data>\n" +
          "\t</page>\n"

      item
    }
  }
}
