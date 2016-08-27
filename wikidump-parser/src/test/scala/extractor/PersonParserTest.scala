package extractor

import java.{util => ju, lang => jl}

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.junit.Assert._
import org.junit.runners.Parameterized.Parameters
import org.scalatest.junit.JUnitSuite

@RunWith(value = classOf[Parameterized])
class PersonParserTest(p1: Object, p2: Object) extends JUnitSuite {

  @Test def test(): Unit = {
    val raw = p1.asInstanceOf[String]
    val expected = p2.asInstanceOf[List[Element]]

    val parsed = new WikiExtractor().parseLine(s"a = ${raw}")
    if (parsed.isEmpty) fail() else parsed.map(_._2).foreach(assertEquals(expected, _))
  }
}

object PersonParserTest {

  @Parameters def parameters: ju.Collection[Array[Object]] = {
    val list = new ju.ArrayList[Array[Object]]()

    list.add(Array("[[King of Aragon]], [[King of Valencia|Valencia]] and [[Count of Barcelona]]",
      List(Link("King of Aragon",None), Sep(","), Link("King of Valencia",Some("Valencia")), Sep("and"), Link("Count of Barcelona",None))))
    list.add(Array("[[Peter III of Aragon|Peter III]]",
      List(Link("Peter III of Aragon",Some("Peter III")))))
    list.add(Array("[[House of Barcelona]]",
      List(Link("House of Barcelona",None))))
    list.add(Array("[[List of monarchs of Naples|King of Naples]] [[List of monarchs of Sicily|and Sicily]]",
      List(Link("List of monarchs of Naples", Some("King of Naples")), Link("List of monarchs of Sicily", Some("and Sicily")))))
    list.add(Array("[[Ferdinand I of Naples]] &lt;small&gt;(illegitimate)&lt;/small&gt;",
      List(Link("Ferdinand I of Naples", None), Text("(illegitimate)")))) // eliminate the '(illegitimate)'
    list.add(Array("[[Alexander IV of Macedon|Alexander IV]]&lt;br /&gt;[[Philip III of Macedon|Philip III]]",
      List(Link("Alexander IV of Macedon", Some("Alexander IV")), Link("Philip III of Macedon", Some("Philip III")))))
    list.add(Array("[[Roxana]] of [[Bactria]]&lt;br /&gt;[[Stateira II]] of [[Persia]]&lt;br /&gt;[[Parysatis II]] of Persia",
      List(Link("Roxana", None), Of("of"), Link("Bactria", None), Link("Stateira II", None), Of("of"), Link("Persia", None), Link("Parysatis II", None), Text("of Persia")))) // postprocess
    list.add(Array("[[Pella]], Macedon",
      List(Link("Pella", None), Text(", Macedon")))) // postprocess
//    list.add(Array("", List()))

    list
  }
}
