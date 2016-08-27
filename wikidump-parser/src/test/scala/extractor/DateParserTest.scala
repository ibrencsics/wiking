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

    val parsed = new WikiExtractor().parseLine(s"a = ${raw}")
    if (parsed.isEmpty) fail() else parsed.map(_._2).foreach(assertEquals(expected, _))
  }
}

object DateParserTest {

  @Parameters def parameters: ju.Collection[Array[Object]] = {
    val list = new ju.ArrayList[Array[Object]]()

    // birth

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
    list.add(Array("1 August AD 12", List(Date("1","August","12",true))))
    list.add(Array("19 August AD 14 (aged 75)", List(Date("19","August","14",true))))
    // 683 or 684


    // circa

    list.add(Array("{{circa|268|232 BCE}}", List(Circa(Timeframe(Date("","","268",false), Date("","","232",false))))))
    list.add(Array("{{circa|340|317 BC}}", List(Circa(Timeframe(Date("","","340",false), Date("","","317",false))))))
    list.add(Array("{{circa|606|647 CE}}", List(Circa(Timeframe(Date("","","606",true), Date("","","647",true))))))

    // list.add(Array("c. 885 to 850 BC", List()))

    list.add(Array("[[circa|c.]] 1068", List(Circa(Date("","","1068",true))))) // postprocessed
    list.add(Array("[[circa|c.]] 1127–1135", List(Circa(Timeframe(Date("","","1127",true), Date("","","1135",true)))))) // postprocessed

    list.add(Array("{{circa}} 1177", List(Circa(Date("","","1177",true))))) // postprocessed
    list.add(Array("{{circa}} 1165–1170", List(Circa(Timeframe(Date("","","1165",true), Date("","","1170",true)))))) // postprocessed
    list.add(Array("{{circa}} 50 BC", List(Circa(Date("","","50",false))))) // postprocessed
    list.add(Array("{{circa}}1074", List(Circa(Date("","","1074",true))))) // postprocessed
    list.add(Array("{{circa}} November 1204", List(Circa(Date("","November","1204",true))))) // postprocessed
    list.add(Array("{{circa}} 870–80", List(Circa(Timeframe(Date("","","870",true), Date("","","880",true)))))) // postprocessed

    list.add(Array("{{circa|1188}}", List(Circa(Date("","","1188",true)))))
    list.add(Array("{{circa|893/895}}", List(Circa(Timeframe(Date("","","893",true), Date("","","895",true))))))
    list.add(Array("{{circa|890s}}", List(Circa(Date("","","890",true)))))
    list.add(Array("{{circa|1182–4}}", List(Circa(Timeframe(Date("","","1182",true), Date("","","1184",true)))))) // postprocessed

    list.add(Array("circa 1161", List(Circa(Date("","","1161",true)))))
    list.add(Array("circa 5 September 1201", List(Circa(Date("5","September","1201",true)))))
    list.add(Array("21 September circa 130", List(Circa(Date("21","September","130",true)))))


    list.add(Array("{{circa| 936}} – {{circa| 958}}",
      List(Circa(Date("","","936",true)), Text("–"), Circa(Date("","","958",true))))) // should be post processed
    list.add(Array("{{circa}} 907&amp;nbsp;– {{circa}} 950 ''(uncertain)''",
      List(Circa(Date("","","907",true)), Circa(Date("","","950",true))))) // postprocessed + should be further postprocessed


    // misc

    // list.add(Array("({{OldStyleDate|28 February|1690|18 February}} | death_date  = {{OldStyleDate|7 July|1718|26 June}}) &lt;br/&gt;(aged 28)", List()))


    // timeframe

    list.add(Array("434–453", List(Timeframe(Date("", "", "434", true), Date("", "", "453", true)))))
    list.add(Array("16 January 27 BC –&lt;br/&gt;19 August AD 14  &lt;small&gt;(40 years)&lt;/small&gt;}}",
      List(Timeframe(Date("16","January","27",false), Date("19","August","14",true)))))

    list.add(Array("{{nowrap|22 June 1691 – 6 February 1695}}", List(Timeframe(Date("22","June","1691",true), Date("6","February","1695",true)))))

    list.add(Array("2 April 1416 – 27 June 1458", List(Timeframe(Date("2","April","1416", true), Date("27","June","1458", true)))))
    list.add(Array("336–323 BC", List(Timeframe(Date("", "", "336", false), Date("", "", "323", false)))))
    list.add(Array("1285–1291", List(Timeframe(Date("", "", "1285", true), Date("", "", "1291", true)))))

    // death

    list.add(Array("27 June {{death year and age|df=yes|1458|1396}}", List(Text("27 June"), Date("","","1458",true)))) // should be post processed
    list.add(Array("10 or 11 June 323 BC (aged 32)&lt;!-- 32 years, 10 months and 20 days (approx.) --&gt;", List(Date("10 or 11","June","323",false))))
    list.add(Array("{{death date and age|df=yes|1670|2|9|1609|3|18}}", List(Date("9","2","1670",true))))
    list.add(Array("February 14, 1009", List(Date("14","February","1009",true))))
    list.add(Array("{{death date and age|df=yes|1621|9|17|1542|10|4}}&lt;br&gt;[[Rome]], Italy", List(Date("17","9","1621",true), Link("Rome", None), Text(", Italy"))))
    list.add(Array("{{death date|df=yes|814|2|18}}", List(Date("18","2","814",true))))
    list.add(Array("{{death date and age|2001|05|13|1906|10|10|df=yes}}", List(Date("13","05","2001",true))))
    list.add(Array("{{BirthDeathAge| |1397| | |1475|12|10|yes}}", List(Timeframe(Date("","","1397",true), Date("10","12","1475",true)))))
    list.add(Array("{{BirthDeathAge||1466|||1530}}", List(Timeframe(Date("","","1466",true), Date("","","1530",true)))))
    list.add(Array("{{BirthDeathAge||1932|4|27|1999|4|18|}}", List(Timeframe(Date("27","4","1932",true), Date("18","4","1999",true)))))
    list.add(Array("{{death-date|April 17, 1994|April 17, 1994}} (aged 80)", List(Date("17","April","1994",true), Text("(aged 80)"))))
    list.add(Array("{{death-date|1806}} (aged 35)", List(Date("","","1806",true), Text("(aged 35)"))))
    list.add(Array("{{Death-date and age|August 3, 1929|May 20, 1851}}", List(Date("20","May","1851",true))))
    list.add(Array("24 January AD 41 (age 28)", List(Date("24","January","41",true))))
    list.add(Array("{{circa|{{death year and age|1353|1291}}}}", List((Date("","","1353",true))))) // should be wrapped to Circa()

    list
  }
}
