package extractor

import java.{util => ju, lang => jl}

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.junit.Assert._
import org.junit.runners.Parameterized.Parameters
import org.scalatest.junit.JUnitSuite

@RunWith(value = classOf[Parameterized])
class PostProcessTest(p1: Object, p2: Object) extends JUnitSuite {

  @Test def test(): Unit = {
    val raw = p1.asInstanceOf[String]
    val expected = p2.asInstanceOf[List[Element]]

    val parsed = new WikiExtractor().parseLine(s"a = ${raw}")
    if (parsed.isEmpty) fail() else parsed.map(_._2).foreach(assertEquals(expected, _))
  }
}

object PostProcessTest {

  @Parameters def parameters: ju.Collection[Array[Object]] = {
    val list = new ju.ArrayList[Array[Object]]()

    list.add(Array("[[circa|c.]] 1068",
//      List(Link("circa",Some("c.")), Date("","","1068",true))))
      List(Circa(Date("","","1068",true)))))
    list.add(Array("[[circa|c.]] 1127–1135",
//      List(Link("circa",Some("c.")), Timeframe(Date("","","1127",true), Date("","","1135",true)))))
      List(Circa(Timeframe(Date("","","1127",true), Date("","","1135",true))))))

    list
  }
}
