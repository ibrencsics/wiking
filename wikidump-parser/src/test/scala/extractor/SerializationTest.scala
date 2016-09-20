package extractor

import org.hamcrest.CoreMatchers.is
import org.junit.Assert.assertThat
import org.junit.Assert.assertEquals
import org.junit.Test

class SerializationTest {

  @Test def test(): Unit = {

    assertThat(Date("20","January","1555",true).toString, is("20.01.1555"))
    assertThat(Date("20.01.1555").toString, is("20.01.1555"))

    assertThat(Date("2","february","155",true).toString, is("02.02.155"))
    assertThat(Date("02.02.155").toString, is("02.02.155"))

    assertThat(Date("2","March","34",false).toString, is("02.03.34 BC"))
    assertThat(Date("02.03.34 BC").toString, is("02.03.34 BC"))
    assertThat(Date("02.03.34 BC"), is(Date("2","March","34",false)))

    assertThat(Date("","","234",true).toString(), is("234"))
    assertThat(Date("234"), is(Date("","","234",true)))

    assertThat(Date("","03","234",true).toString(), is("03.234"))
    assertThat(Date("03.234"), is(Date("","03","234",true)))

    assertThat(Date("","03","234",false).toString(), is("03.234 BC"))
    assertThat(Date("03.234 BC"), is(Date("","03","234",false)))
  }
}
