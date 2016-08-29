package extractor

import extractor.FreeText.Free

import scala.collection.mutable

// https://gist.github.com/kencoba/1874015
class RoyaltyBuilder {
  var name: Free = null
  var succession = mutable.Map[Integer, Free]()
  var predecessor = mutable.Map[Integer, Free]()
  var successor = mutable.Map[Integer, Free]()
  var reign = mutable.Map[Integer, Free]()
  var spouse: Free = null
  var issue: Free = null
  var house: Free = null
  var father: Free = null
  var mother: Free = null
  var birthDate: Free = null
  var birthPlace: Free = null
  var deathDate: Free = null
  var deathPlace: Free = null
  var religion: Free = null

  def withName(name: Free): RoyaltyBuilder = {
    this.name = name
    this
  }

  def withSuccession(i: Integer, succession: Free): RoyaltyBuilder = {
    this.succession += (i -> succession)
    this
  }

  def withPredecessor(i: Integer, predecessor: Free): RoyaltyBuilder = {
    this.predecessor += (i -> predecessor)
    this
  }

  def withSuccessor(i: Integer, successor: Free): RoyaltyBuilder = {
    this.successor += (i -> successor)
    this
  }

  def withReign(i: Integer, reign: Free): RoyaltyBuilder = {
    this.reign += (i -> reign)
    this
  }

  def withSpouse(spouse: Free): RoyaltyBuilder = {
    if (this.spouse != null) this.spouse = this.spouse ::: spouse
    else this.spouse = spouse
    this
  }

  def withIssue(issue: Free): RoyaltyBuilder = {
    this.issue = issue
    this
  }

  def withHouse(house: Free): RoyaltyBuilder = {
    this.house = house
    this
  }

  def withFather(father: Free): RoyaltyBuilder = {
    this.father = father
    this
  }

  def withMother(mother: Free): RoyaltyBuilder = {
    this.mother = mother
    this
  }

  def withBirthDate(birthDate: Free): RoyaltyBuilder = {
    this.birthDate = birthDate
    this
  }

  def withBirthPlace(birthPlace: Free): RoyaltyBuilder = {
    this.birthPlace = birthPlace
    this
  }

  def withDeathDate(deathDate: Free): RoyaltyBuilder = {
    this.deathDate = deathDate
    this
  }

  def withDeathPlace(deathPlace: Free): RoyaltyBuilder = {
    this.deathPlace = deathPlace
    this
  }

  def withReligion(religion: Free): RoyaltyBuilder = {
    this.religion = religion
    this
  }

  def build: Royalty = new Royalty(this)
}

class Royalty(builder: RoyaltyBuilder) {
  var name = builder.name
  var succession: Map[Integer, Free] = builder.succession.toMap
  var predecessor: Map[Integer, Free] = builder.predecessor.toMap
  var successor: Map[Integer, Free] = builder.successor.toMap
  var reign: Map[Integer, Free] = builder.reign.toMap
  var spouse = builder.spouse
  var issue = builder.issue
  var house = builder.house
  var father = builder.father
  var mother = builder.mother
  var birthDate = builder.birthDate
  var birthPlace = builder.birthPlace
  var deathDate = builder.deathDate
  var deathPlace = builder.deathPlace
  var religion = builder.religion

  def canEqual(a: Any) = a.isInstanceOf[Royalty]

  override def equals(that: Any): Boolean = that match {
    case that: Royalty => {
      that.canEqual(this) &&
      that.name == this.name &&
      that.succession == this.succession &&
      that.predecessor == this.predecessor &&
      that.successor == this.successor &&
      that.reign == this.reign &&
      that.spouse == this.spouse &&
      that.issue == this.issue &&
      that.house == this.house &&
      that.father == this.father &&
      that.mother == this.mother &&
      that.birthDate == this.birthDate &&
      that.birthPlace == this.birthPlace &&
      that.deathDate == this.deathDate &&
      that.deathPlace == this.deathPlace &&
      that.religion == this.religion
    }
    case _ => false
  }

  override def toString: String = {
    s"name: ${name}\n" +
    s"succession: ${succession}\n" +
    s"predecessor: ${predecessor}\n" +
    s"sucessor: ${successor}\n" +
    s"reign: ${reign}\n" +
    s"spouse: ${spouse}\n" +
    s"issue: ${issue}\n" +
    s"house: ${house}\n" +
    s"father: ${father}\n" +
    s"mother: ${mother}\n" +
    s"birthDate: ${birthDate}\n" +
    s"birthPlace: ${birthPlace}\n" +
    s"deathDate: ${deathDate}\n" +
    s"deathPlace: ${deathPlace}\n" +
    s"religion: ${religion}\n"
  }
}

object RoyaltyRegexHelper {
  val Succession = """succession(\d*)""".r
  val Predecessor = """predecessor(\d*)""".r
  val Successor = """successor(\d*)""".r
  val Reign = """reign(\d*)""".r
  val Title = """^title(\d*)$""".r
  val Spouse = """^spouse""".r
  val Issue = """^issue$""".r
  val House = """^house$|^dynasty$""".r
  val Father = """^father$""".r
  val Mother = """^mother$""".r
  val Name = """^name$""".r
  val BirthDate = """^birth_date$""".r
  val BirthPlace = """^birth_place$""".r
  val DeathDate = """^death_date$""".r
  val DeathPlace = """^death_place$""".r
  val Religion = """^religion$""".r
}

import extractor.WikiExtractor.getNum

import scala.collection.mutable.ListBuffer

object Succession {
  def unapply(raw: String): Option[Int] = getNum(RoyaltyRegexHelper.Succession, raw)
}

object Predecessor {
  def unapply(raw: String): Option[Int] = getNum(RoyaltyRegexHelper.Predecessor, raw)
}

object Successor {
  def unapply(raw: String): Option[Int] = getNum(RoyaltyRegexHelper.Successor, raw)
}

object Reign {
  def unapply(raw: String): Option[Int] = getNum(RoyaltyRegexHelper.Reign, raw)
}

object Title {
  def unapply(raw: String): Option[String] = RoyaltyRegexHelper.Title.findFirstIn(raw)
}

object Spouse {
  def unapply(raw: String): Option[String] = RoyaltyRegexHelper.Spouse.findFirstIn(raw)
}

object Issue {
  def unapply(raw: String): Option[String] = RoyaltyRegexHelper.Issue.findFirstIn(raw)
}

object House {
  def unapply(raw: String): Option[String] = RoyaltyRegexHelper.House.findFirstIn(raw)
}

object Father {
  def unapply(raw: String): Option[String] = RoyaltyRegexHelper.Father.findFirstIn(raw)
}

object Mother {
  def unapply(raw: String): Option[String] = RoyaltyRegexHelper.Mother.findFirstIn(raw)
}

object Name {
  def unapply(raw: String): Option[String] = RoyaltyRegexHelper.Name.findFirstIn(raw)
}

object BirthDate {
  def unapply(raw: String): Option[String] = RoyaltyRegexHelper.BirthDate.findFirstIn(raw)
}

object BirthPlace {
  def unapply(raw: String): Option[String] = RoyaltyRegexHelper.BirthPlace.findFirstIn(raw)
}

object DeathDate {
  def unapply(raw: String): Option[String] = RoyaltyRegexHelper.DeathDate.findFirstIn(raw)
}

object DeathPlace {
  def unapply(raw: String): Option[String] = RoyaltyRegexHelper.DeathPlace.findFirstIn(raw)
}

object Religion {
  def unapply(raw: String): Option[String] = RoyaltyRegexHelper.Religion.findFirstIn(raw)
}


class RoyaltyExtractor {

  def extract(infobox: String): Royalty = {

    val builder = new RoyaltyBuilder
    val template = new WikiExtractor().parseTemplate(infobox)

    for (line <- template.data) {
      line match {
        case (Name(i), elements) => builder.withName(elements)
        case (Succession(i), elements) => builder.withSuccession(i, elements)
        case (Predecessor(i), elements) => builder.withPredecessor(i, elements)
        case (Successor(i), elements) => builder.withSuccessor(i, elements)
        case (Reign(i), elements) => builder.withReign(i, elements)
        case (Spouse(str), elements) => builder.withSpouse(elements)
        case (Issue(str), elements) => builder.withIssue(elements)
        case (House(str), elements) => builder.withHouse(elements)
        case (Father(str), elements) => builder.withFather(elements)
        case (Mother(str), elements) => builder.withMother(elements)
        case (BirthDate(str), elements) => builder.withBirthDate(elements)
        case (BirthPlace(str), elements) => builder.withBirthPlace(elements)
        case (DeathDate(str), elements) => builder.withDeathDate(elements)
        case (DeathPlace(str), elements) => builder.withDeathPlace(elements)
        case (Religion(str), elements) => {builder.withReligion(elements)}
        case _ => //println("no match")
      }
    }

    builder.build
  }
}