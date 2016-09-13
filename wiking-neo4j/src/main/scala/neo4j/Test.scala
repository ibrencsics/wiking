package neo4j

import org.neo4j.driver.v1.{AuthTokens, GraphDatabase}
import scala.collection.JavaConversions._

object Test extends App {

  val driver = GraphDatabase.driver("bolt://localhost/7687", AuthTokens.basic("neo4j", "24494897"))
  val session = driver.session


  val user = User("Ivan", "Brencsics", 37, "München")

//  val script = s"CREATE (user:Users {name:'${user.name}',last_name:'${user.last_name}',age:${user.age},city:'${user.city}'})"
//  val result = session.run(script)

  val script2 = "MATCH (user:Users) RETURN user.name AS name, user.last_name AS last_name, user.age AS age, user.city AS city"
  val result2 = session.run(script2)
  for (r <- result2.list()) {
    println(r)
  }
}

case class User(name: String, last_name: String, age: Int, city: String)