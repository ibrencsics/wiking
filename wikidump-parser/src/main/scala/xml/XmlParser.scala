package xml

import scala.collection.mutable.ArrayBuffer
import scala.xml.pull._

class XmlParser {

  var insidePage = false
  var buf = ArrayBuffer[String]()

  def parse(xml: XMLEventReader, processor: ArrayBuffer[String] => Unit): Unit = {

    for (event <- xml) {
      event match {
        case EvElemStart(_, "page", _, _) => {
          insidePage = true
          val tag = "<page>"
          buf += tag
        }
        case EvElemEnd(_, "page") => {
          val tag = "</page>"
          buf += tag
          insidePage = false

          processor(buf)
          buf.clear
        }
        case e @ EvElemStart(_, tag, _, _) => {
          if (insidePage) {
            buf += ("<" + tag + ">")
          }
        }
        case e @ EvElemEnd(_, tag) => {
          if (insidePage) {
            buf += ("</" + tag + ">")
          }
        }
        case EvText(t) => {
          if (insidePage) {
            buf += t
          }
        }
        case EvEntityRef(t) => {
          if (insidePage) {
            buf += "&" + t + ";"
          }
        }
        case _ => // ignore
      }
    }
  }
}
