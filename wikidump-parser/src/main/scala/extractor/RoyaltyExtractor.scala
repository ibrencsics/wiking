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
    this.spouse = spouse
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
//  val Reign = """reign(?![-\w])(\d*)""".r
  val Reign = """reign(\d*)""".r
  val Title = """^title(\d*)$""".r
  val Spouse = """^spouse$""".r
  val Issue = """^issue$""".r
  val House = """^house$""".r
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

object RoyaltyExtractor {
  def main(args: Array[String]) {
//    val template = new WikiExtractor().parseTemplate(
    val template =
            "{{infobox royalty\\n| name         = Alfonso III\\n| image        = Jaume Mateu - Alfons III the Liberal - Google Art Project.jpg\\n| succession   = [[King of Aragon]], [[King of Valencia|Valencia]] and [[Count of Barcelona]] \\n| reign        = 1285–1291\\n| coronation   = 2 February 1286 (Valencia)&lt;br /&gt;9 April 1286 (Zaragoza)\\n| predecessor  = [[Peter III of Aragon|Peter III]]\\n| successor     = [[James II of Aragon|James II]]\\n| spouse       = \\n| issue        = \\n| house        = [[House of Barcelona]]\\n| father       = [[Peter III of Aragon]]\\n| mother       = [[Constance of Sicily, Queen of Aragon|Constance of Sicily]]\\n| birth_date   = 4 November 1265\\n| birth_place  = [[Valencia]]\\n| death_date   = 18 June 1291 (aged 26)\\n| death_place  = [[Barcelona]]\\n| burial_date  =\\n| burial_place = [[Barcelona Cathedral]]; prev. Convent de San Francisco, Barcelona\\n| religion     = [[Catholic Church|Roman Catholicism]]\\n}}"
      //    "{{Infobox royalty\n| type         = monarch\n| name         = Alfonso the Magnanimous\n| image        = Alfonso-V-el-Magnanimo.jpg\n| caption      = Portrait of Alfonso V of Aragon, by 16th century painter [[Vicente Juan Masip]]\n| spouse       = [[Maria of Castile]]\n| succession   = [[Crown of Aragon|King of Aragon]]\n| reign        = 2 April 1416 – 27 June 1458\n| predecessor  = [[Ferdinand I of Aragon|Ferdinand I]]\n| successor    = [[John II of Aragon|John II]]\n| succession1  = [[List of monarchs of Naples|King of Naples]] [[List of monarchs of Sicily|and Sicily]]\n| reign1       = 2 June 1442 – 27 June 1458\n| predecessor1 = [[René of Anjou|René]]\n| successor1   = [[Ferdinand I of Naples|Ferdinand I]]\n| issue        = [[Ferdinand I of Naples]] &lt;small&gt;(illegitimate)&lt;/small&gt;\n| issue-link   = #Family\n| issue-pipe   = among others...\n| house        = [[House of Trastámara]]\n| father       = [[Ferdinand I of Aragon]]\n| mother       = [[Eleanor of Alburquerque]]\n| birth_date   = 1396\n| birth_place  = [[Medina del Campo]], [[Kingdom of Castile]]\n| death_date   = 27 June {{death year and age|df=yes|1458|1396}}\n| death_place  = [[Castel dell'Ovo]], [[Naples]], [[Kingdom of Naples]]\n| burial_date  = \n| burial_place = [[Poblet Monastery]]\n| religion     = [[Catholic Church|Roman Catholicism]]\n}}"
      //      "{{Infobox royalty\n| name = Alexander the Great\n| title = [[Basileus]] of [[Macedon]], [[Hegemony|Hegemon]] of the [[League of Corinth|Hellenic League]], [[Shahanshah]] of [[Persia]], [[Pharaoh]] of [[Ancient Egypt|Egypt]], [[Lord of Asia]]\n| image = Alexander the Great mosaic.jpg\n| caption = &lt;small&gt;{{Citation | contribution = Alexander fighting king [[Darius III of Persia]] | title = [[Alexander Mosaic]] | publisher = [[Naples National Archaeological Museum]]}}.&lt;/small&gt;\n| succession   = [[List of kings of Macedon|King of Macedonia]]\n| reign = 336–323 BC\n| predecessor  = [[Philip II of Macedon|Philip II]]\n| successor = [[Alexander IV of Macedon|Alexander IV]]&lt;br /&gt;[[Philip III of Macedon|Philip III]]\n| succession1   = [[Pharaoh|Pharaoh of Egypt]]\n| reign1 = 332–323 BC\n| predecessor1  = [[Darius III]]\n| successor1 = [[Alexander IV of Macedon|Alexander IV]]&lt;br /&gt;[[Philip III of Macedon|Philip III]]\n| succession2   = [[List of kings of Persia|King of Persia]]\n| reign2 = 330–323 BC\n| predecessor2  = [[Darius III]]\n| successor2 = [[Alexander IV of Macedon|Alexander IV]]&lt;br /&gt;[[Philip III of Macedon|Philip III]]\n| succession3   = [[Lord of Asia|King of Asia]]\n| reign3 = 331–323 BC\n| predecessor3  = ''New office''\n| successor3 = [[Alexander IV of Macedon|Alexander IV]]&lt;br /&gt;[[Philip III of Macedon|Philip III]]\n| othertitles =\n| full name = Alexander III of Macedon\n| native_lang1 = [[Greek language|Greek]]\n| native_lang1_name1 ={{plainlist |\n* Μέγας Ἀλέξανδρος{{Cref2|d}} (''Mégas Aléxandros'', Great Alexander)\n* Ἀλέξανδρος ὁ Μέγας (''Aléxandros ho Mégas'', Alexander the Great)\n}}\n| spouse = [[Roxana]] of [[Bactria]]&lt;br /&gt;[[Stateira II]] of [[Persia]]&lt;br /&gt;[[Parysatis II]] of Persia\n| issue = [[Alexander IV of Macedon|Alexander IV]]\n| house = [[Argead dynasty|Argead]]\n| house-type = Dynasty\n| father = [[Philip II of Macedon]]\n| mother = [[Olympias|Olympias of Epirus]]\n| birth_date = 20 or 21 July 356 BC\n| birth_place = [[Pella]], Macedon\n| death_date = 10 or 11 June 323 BC (aged 32)&lt;!-- 32 years, 10 months and 20 days (approx.) --&gt;\n| death_place = [[Babylon]]\n| religion = [[Religion in ancient Greece|Greek polytheism]]}}"
      //      "{{Infobox royalty\n| type            = monarch\n| name            = Abd al-Rahman I\n| title           = \n| image_size            = 250\n| image            = Abdul al Rahman I.jpg\n| caption         =\n| predecessor     = [[Yusuf ibn 'Abd al-Rahman al-Fihri]] (as governor of al-Andalus)\n| succession      = 1st [[Emir of Córdoba]]\n| reign           = 756–788\n| coronation      = \n| predecessor1    = \n| successor1      = [[Hisham I of Córdoba|Hisham I]]\n| spouse          = Hulal\n| spouse-type     = Spouse\n| issue           = Sulayman&lt;br&gt;Omar&lt;br&gt;[[Hisham I of Córdoba|Hisham I]]&lt;br&gt;Abdallah\n| house           = [[Umayyad dynasty|Umayyad]]\n| house-type      = Dynasty\n| father          = [[Mu'awiya ibn Hisham]]\n| mother          = Ra'ha, Berber concubine, Nafza tribe&lt;ref&gt;''Granada: A Case Study of Arab Urbanism in Muslim Spain'', James Dickie, '''The Legacy of Muslim Spain''', ed. Salma Khadra Jayyusi and Manuela Marín, (Brill, 1994), 19.&lt;/ref&gt;\n| birth_date   = 731\n| birth_place  = Palmyra, near [[Damascus]], [[Syria (region)|ash-Sham]]\n| death_date   = 788\n| death_place  = [[Córdoba, Spain|Córdoba]], [[Al-Andalus]]\n| place of burial = \n| signature       = \n| religion        = [[Islam]]\n}}"
      //      "{{Infobox royalty\n| name         = Abdülaziz&lt;br /&gt;عبد العزيز\n| title        = [[Ottoman Caliphate|Caliph of Islam]]&lt;br/&gt;[[Amir al-Mu'minin]]&lt;br/&gt;[[List of sultans of the Ottoman Empire|Sultan of]] the [[Ottoman Empire]]&lt;br/&gt;[[Custodian of the Two Holy Mosques]]\n| titletext    = \n| more         = \n| type         = \n| image        = Sultan Abdulaziz of the Ottoman Empire.jpg\n| alt          = \n| caption      = Sultan Abdülaziz during his visit to the [[United Kingdom of Great Britain and Ireland|United Kingdom]] in 1867\n| moretext     = \n| reign        = 2 June 1861 – 30 May 1876\n| reign-type   = Reign\n| coronation   = \n| cor-type     = \n| predecessor  = [[Abdülmecid I]]\n| pre-type     = Predecessor\n| regent       = \n| reg-type     = \n| successor    = [[Murad V]]\n| suc-type     = Successor\n| succession   = 24th [[Caliph]] of the [[Ottoman Caliphate]]&lt;br /&gt;32nd [[List of sultans of the Ottoman Empire|Ottoman Sultan]] ([[Padishah|Emperor]])\n| spouse       = [[Dürrinev Kadınefendi]]&lt;br /&gt;[[Edadil Kadınefendi]]&lt;br /&gt;[[Hayranidil Kadınefendi]]&lt;br /&gt;[[Neşerek Kadınefendi]]&lt;br /&gt;[[Gevheri Kadınefendi]]&lt;br /&gt;Yildiz Hanimefendi\n| spouse-type  = Consorts\n| consort      = yes\n| issue        = \n| full name    = \n| house        = [[House of Osman|Osmanli]] (''Ottoman'')\n| house-type   = Dynasty\n| father       = [[Mahmud II]]\n| mother       = [[Pertevniyal Sultan]]\n| birth_date   = 9 February 1830\n| birth_place  = [[Constantinople]]\n| death_date   = {{death date and age|1876|6|4|1830|2|9|df=y}}&lt;ref name=EB&gt;{{cite encyclopedia |editor-first=Dale H. |editor-last=Hoiberg|encyclopedia=Encyclopædia Britannica |title=Abdülaziz |edition=15th |year=2010|publisher=Encyclopædia Britannica Inc. |volume=I: A-ak Bayes |location=Chicago, IL |isbn=978-1-59339-837-8|pages=21}}&lt;/ref&gt;\n| death_place  = [[Çırağan Palace]]\n| burial_date  = \n| burial_place = \n| religion= [[Sunni Islam]]\n| signature_type = [[Tughra]]\n| signature    = Tughra of Abdülaziz.JPG\n}}"


    new RoyaltyExtractor().extract(template)
  }
}