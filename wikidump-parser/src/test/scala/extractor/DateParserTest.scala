package extractor

import java.{util => ju, lang => jl}

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.junit.Assert._
import org.junit.runners.Parameterized.Parameters
import org.scalatest.junit.JUnitSuite

@RunWith(value = classOf[Parameterized])
class DateParserTest(p1: Object, p2: Object) extends JUnitSuite {

  @Test def test(): Unit = {
    val raw = p1.asInstanceOf[String]
    val expected = p2.asInstanceOf[List[Element]]

    raw match {
      case FreeText(elemValue) => assertEquals(expected, elemValue)
      case t => fail()
    }
  }
}

object DateParserTest {

  @Parameters def parameters: ju.Collection[Array[Object]] = {
    val list = new ju.ArrayList[Array[Object]]()

    list.add(Array("{{birth date|1809|2|12}}", List(Date("12","2","1809",true))))
    list.add(Array("{{birth_date|1460|5|8|df=yes}}", List(Date("8","5","1460",true))))
    list.add(Array("{{birth date|1912|4|26|mf=y}}", List(Date("26","4","1912",true))))
    list.add(Array("{{Birth date|df=yes|1885|4|3}}", List(Date("3","4","1885",true))))
    list.add(Array("{{birth date and age|1947|04|01|df=y}}", List(Date("01","04","1947",true))))

    list.add(Array("{{Birth-date|August 20, 2009|August 20, 1913}}", List(Date("20","August","1913",true))))
    list.add(Array("{{Birth-date|11 September 1771}}", List(Date("11","September","1771",true))))
    list.add(Array("{{Birth-date|May 20, 1851|May 20, 1851}}", List(Date("20","May","1851",true))))

    list.add(Array("{{BirthDeathAge|B|1397| | |1475|12|10|yes}}", List(Timeframe(Date("","","1397",true), Date("10","12","1475",true)))))
    list.add(Array("{{BirthDeathAge|B|1466|||1530}}", List(Timeframe(Date("","","1466",true), Date("","","1530",true)))))
    list.add(Array("{{BirthDeathAge|B|1932|4|27|1999|4|18|}}", List(Timeframe(Date("27","4","1932",true), Date("18","4","1999",true)))))

    list.add(Array("4 November 1265", List(Date("4","November","1265", true))))
    list.add(Array("20 or 21 July 356 BC", List(Date("20 or 21","July","356",false))))

    list.add(Array("1396", List(Date("","","1396",true))))
//    list.add(Array("1 August AD 12", List()))
//    list.add(Array("19 August AD 14 (aged 75)", List())

//    list.add(Array("{{circa|268|232 BCE}}", List()))
//    list.add(Array("{{circa|340|317 BC}}", List()))
//    list.add(Array("{{circa|606|647 CE}}", List()))

//    list.add(Array("c. 885 to 850 BC", List()))

    list.add(Array("[[circa|c.]] 1068", List(Link("circa",Some("c.")), Date("","","1068",true)))) // should be post processed
    list.add(Array("[[circa|c.]] 1127–1135", List(Link("circa",Some("c.")), Timeframe(Date("","","1127",true), Date("","","1135",true))))) // should be post processed

    list.add(Array("{{circa}} 1177", List(Circa(null), Date("","","1177",true))))
    list.add(Array("{{circa}} 1165–1170", List(Circa(null), Timeframe(Date("","","1165",true), Date("","","1170",true)))))
    list.add(Array("{{circa}} 50 BC", List(Circa(null), Date("","","50",false))))
    list.add(Array("{{circa}}1074", List(Circa(null), Date("","","1074",true))))
    list.add(Array("{{circa}} November 1204", List(Circa(null), Date("","November","1204",true))))
    list.add(Array("{{circa}} 870–80", List(Circa(null), Timeframe(Date("","","870",true), Date("","","80",true))))) // should be post processed

    list.add(Array("{{circa|1188}}", List(Circa(Date("","","1188",true)))))
    list.add(Array("{{circa|893/895}}", List(Circa(Timeframe(Date("","","893",true), Date("","","895",true))))))
    list.add(Array("{{circa|890s}}", List(Circa(Date("","","890",true)))))
    list.add(Array("{{circa|1182–4}}", List(Circa(Timeframe(Date("","","1182",true), Date("","","4",true)))))) // should be post processed

//    list.add(Array("circa 1161", List()))
//    list.add(Array("circa 5 September 1201", List()))
//    list.add(Array("21 September circa 130", List()))


//    list.add(Array("{{circa| 936}} – {{circa| 958}}", List()))
//    list.add(Array("{{circa}} 907&amp;nbsp;– {{circa}} 950 ''(uncertain)''", List()))


//    list.add(Array("({{OldStyleDate|28 February|1690|18 February}} | death_date  = {{OldStyleDate|7 July|1718|26 June}}) &lt;br/&gt;(aged 28)", List()))


//    list.add(Array("27 June {{death year and age|df=yes|1458|1396}}", List(Date("","","1458",true))))

    list.add(Array("434–453", List(Timeframe(Date("", "", "434", true), Date("", "", "453", true)))))
//    list.add(Array("16 January 27 BC –&lt;br/&gt;19 August AD 14  &lt;small&gt;(40 years)&lt;/small&gt;}}", List()))
//    list.add(Array("{{nowrap|22 June 1691 – 6 February 1695}}", List()))

    list
  }
}
