package neo4j

import java.sql.{DriverManager, PreparedStatement, Connection}
import java.util.{Date => JDate}

import extractor.{Link, Date, PageExtractor, RoyaltyExtractor}
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

  val driver = "com.mysql.jdbc.Driver"
  val url = "jdbc:mysql://localhost:6603/wikipedia"
  val username = "root"
  val password = "mypassword"
  Class.forName(driver)
  var connection:Connection = null
  var statement: PreparedStatement = null

  implicit val wsclient = ning.NingWSClient()
  implicit val connection2: Neo4jConnection = Neo4jREST("localhost", 7474, "neo4j", "24494897")
  implicit val ec = scala.concurrent.ExecutionContext.global

//  Cypher("MATCH (n) DETACH DELETE n")()

  try {

    connection = DriverManager.getConnection(url, username, password)
    connection.setAutoCommit(false)
    statement = connection.prepareStatement("select redirect from redirects where alias=?")

    val pre = new JDate()
    new XmlParser().parse(inputXml, persistToNeo)
    val post = new JDate()

  } finally {
    wsclient.close
    statement.close
    connection.close
  }


  def persistToNeo(buf: ArrayBuffer[String]): Unit = {

    val s = buf.mkString
    val x = XML.loadString(s)

    val pageTitle = (x \ "name") (0).child(0).toString
    var realPageTitle = pageTitle
    val infoboxType = (x \ "type") (0).child(0).toString.toLowerCase.trim
    val infobox = (x \ "text") (0).child(0).toString.trim

    val royalty = new RoyaltyExtractor().extract(infobox)

    statement.setString(1, pageTitle)
    val res = statement.executeQuery()

    if (res.next()) {
      realPageTitle = res.getString(1)
      println(s"------- ${pageTitle} ---------- ${realPageTitle}")
    }

    var birthDate: String = null
    royalty.birthDate.head match {
      case Date(d, m, y, a) => birthDate = s"${d}.${m}.${y}"
      case _ =>
    }


    val Tag = "Person"

//    val resCipher = Cypher()()

    Cypher(
      s"""merge (n:${Tag}{name: "${realPageTitle}"})
         |on match set n += {birthDate: "${birthDate}"}
         |on create set n += {birthDate: "${birthDate}"}""".stripMargin)()


    royalty.father.foreach{case Link(p,a) => Cypher(
      s"""merge (father:${Tag}{name: "${p}"})
         |merge (child:${Tag}{name: "${realPageTitle}"})
         |merge (child)-[:HAS_FATHER]->(father)""".stripMargin)()}

//    val res2 = Cypher(s"""match (n:${Tag}{name: "${pageTitle}"}) return n.name""")()
//    println(res2.map(row => {row[String]("n.name")}).toList)
  }
}
