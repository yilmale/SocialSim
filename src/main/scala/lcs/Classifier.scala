package lcs
import Constants._

class Condition {
  var data : Option[BitString] = None

  def set(bits: String): Unit = {
    data = Some(BitString(bits) )
  }
}

object Condition {
  def apply(bits:String): Condition = {
    val cnd : Condition = new Condition()
    cnd.set(bits)
    cnd
  }
}

class Phenotype {
  var data : Option[BitString] = None

  def set(bits: String): Unit = {
    data = Some(BitString(bits) )
  }
}

object Phenotype {
  def apply(bits:String): Phenotype = {
    val ptype : Phenotype = new Phenotype()
    ptype.set(bits)
    ptype
  }
}

class Classifier(setSize: Int, situation: Condition, act: Phenotype, time: Int)
  extends ClassifierRule[Condition,Phenotype,Boolean] {
  var condition: Condition = situation
  var action: Phenotype = act
  var fitness: Double = initialFitness
  var accuracy: Double = 0.0
  var matchCount: Int = 0
  var correctCount: Int = 0
  var iniTimeStamp: Int = time
  var timeStampGA: Int = 0
  var numerosity : Int = 1
  var avgMatchSize : Int = setSize

  def conditionMatch(state: Condition): Boolean = {
    val ruleCond: String = condition.data.get.bits
    val st: String = state.data.get.bits
    for (i <- 0 until ruleCond.length) {
      if ((ruleCond(i) != '#') && (ruleCond(i) != st(i)))
        return false
    }
    true
  }

  def report(): String = {
    var cnd: String = condition.data.get.bits
    var act: String = action.data.get.bits
    s"Rule : \nCondition: $cnd " + s"\nAction: $act" + s"\nAccuracy: $accuracy" + s"\nFitness: $fitness" +
      s"\nMatch Count: $matchCount" + s"\nCorrect Count: $correctCount" + s"\nTime Stamp: $iniTimeStamp" +
      s"\nNumerosity: $numerosity"
  }

  def getPhenotypeData: String = {
    action.data.get.bits
  }

  def getConditionData: String = {
    condition.data.get.bits
  }

  def getWCCardinality: Int = {
    condition.data.get.wc.cardinality()
  }

  def isMoreGeneralThan: ClassifierRule[Condition,Phenotype,Boolean] => Boolean = {cl =>
    var isMoreGeneral : Boolean = true
    if (condition.data.get.wc.cardinality() <= cl.condition.data.get.wc.cardinality())
      isMoreGeneral = false
    if (isMoreGeneral) {
      var i : Int = 0
      do {
        if ((condition.data.get.bits(i) != '#') &&
          (condition.data.get.bits(i) != cl.condition.data.get.bits(i)))
          isMoreGeneral = false
        i = i + 1
      } while ((i < condition.data.get.bits.length) && (isMoreGeneral))
    }
    isMoreGeneral
  }

}



object ClassifierCover {
  def apply(setSize: Int, situation: Condition, act: Phenotype, time: Int): Classifier = {
    val cnd : Condition = new Condition()
    val context : String = situation.data.get.bits
    var str = new StringBuilder("")

    for (i <- 0 until context.length) {
      if (rng.nextDouble() <= p_wc)
        str = str += '#'
      else
        str = str += context(i)
    }
    cnd.set(str.toString())
    new Classifier(setSize,cnd, act, time)
  }
}

object ClassifierGen {
  def apply(cl: Classifier): Classifier = {
    val gen = new Classifier(1,
      Condition(cl.condition.data.get.bits),
      Phenotype(cl.action.data.get.bits),
      cl.timeStampGA
    )
    gen.timeStampGA = cl.timeStampGA
    gen.matchCount = cl.matchCount
    gen.correctCount = cl.correctCount
    gen.avgMatchSize = cl.avgMatchSize
    gen
  }
}



