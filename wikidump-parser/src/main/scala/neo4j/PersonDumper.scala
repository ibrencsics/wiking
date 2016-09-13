package neo4j

import java.util.Date

import extractor.{PageExtractor, RoyaltyExtractor}
import org.anormcypher._
import play.api.libs.ws._
import xml.XmlParser

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.xml.XML
import scala.xml.pull.XMLEventReader

object PersonDumper extends App {

  val inputFile = "/home/ivan/opt/wikidump/enwiki/neotest.xml"
  val inputXml = new XMLEventReader(Source.fromFile(inputFile))

  implicit val wsclient = ning.NingWSClient()
  implicit val connection2: Neo4jConnection = Neo4jREST("localhost", 7474, "neo4j", "24494897")
  implicit val ec = scala.concurrent.ExecutionContext.global

  val pre = new Date()
  new XmlParser().parse(inputXml, persistToNeo)
  val post = new Date()

  wsclient.close


  def persistToNeo(buf: ArrayBuffer[String]) = {

    val s = buf.mkString
    val x = XML.loadString(s)

    val pageTitle = (x \ "name") (0).child(0).toString
    val infoboxType = (x \ "type") (0).child(0).toString.toLowerCase.trim
    val infobox = (x \ "text") (0).child(0).toString.trim

    val royalty = new RoyaltyExtractor().extract(infobox)

//    println(royalty.father)
//    println(royalty.mother)
//    println(royalty.issue)
//    println(royalty.spouse)
//    println()

    val Tag = "Person"

//    val res1 = Cypher(s"""create (n:${Tag}{name: "${pageTitle}"})""")
//    res1()

    val res2 = Cypher(s"""match (n:${Tag}{name: "${pageTitle}"}) return n.name""")()
    println(res2.map(row => {row[String]("n.name")}).toList)
  }
}
