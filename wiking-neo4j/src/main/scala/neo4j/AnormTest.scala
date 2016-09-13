package neo4j

import org.anormcypher._
import play.api.libs.ws._

object AnormTest extends App {

  implicit val wsclient = ning.NingWSClient()
  implicit val connection2: Neo4jConnection = Neo4jREST("localhost", 7474, "neo4j", "24494897")

  implicit val ec = scala.concurrent.ExecutionContext.global

//  val result: Boolean = Cypher("START n=node(0) RETURN n").execute()

  val req = Cypher("MATCH (user:Users) RETURN user.name AS name, user.last_name AS last_name, user.age AS age, user.city AS city")

  // get a stream of results back
  val stream = req()

  // get the results and put them into a list
  println(stream.map(row => {row[String]("name")}))

}
