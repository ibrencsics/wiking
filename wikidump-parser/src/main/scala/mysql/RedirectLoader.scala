package mysql

import java.sql.{PreparedStatement, DriverManager, Connection}


object RedirectLoader extends App {

  val driver = "com.mysql.jdbc.Driver"
  val url = "jdbc:mysql://localhost:6603/wikipedia"
  val username = "root"
  val password = "mypassword"

  Class.forName(driver)
  val bufferedSource = io.Source.fromFile("/home/ivan/opt/wikidump/enwiki/redirects.csv")


  var connection:Connection = null
  var statement: PreparedStatement = null

  val batchSize = 10000;
  var count = 0;

  try {
    connection = DriverManager.getConnection(url, username, password)
    connection.setAutoCommit(false)

    statement = connection.prepareStatement("insert into redirects (alias, redirect) values (?, ?)")

    for (line <- bufferedSource.getLines) {
      val cols = line.split(";").map(_.trim)


      statement.setString(1, cols(0))
      statement.setString(2, cols(1))
      statement.addBatch()

      count += 1
      if(count % batchSize == 0) {
        statement.clearBatch()
        println(count)
      }

//      println(s"${cols(0)}|${cols(1)}")
    }

    statement.executeBatch()
    connection.commit()

  } catch {
    case e => e.printStackTrace
  }

  statement.close
  connection.close
  bufferedSource.close
}
