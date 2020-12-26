package xcs

import Constants._
import lcs.BitString

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


class Classifier(situation: Condition,time: Int) extends ClassifierRule[Condition,Phenotype,Double] {
  var condition: Condition = situation
  var action: Phenotype = _
  var prediction : Double  = 0.0
  var fitness: Double = initialFitness
  var error: Double = 0.0
  var experience : Int = 0
  var timeStamp : Int = time
  var actionSetSize : Double = 1.0
  var accuracy: Double = 0.0
  var timeStampGA: Int = time
  var numerosity : Int = 1

  def conditionMatch(state: Condition): Boolean = {
    val ruleCond: String = condition.data.get.bits
    val st: String = state.data.get.bits
    for (i <- 0 until ruleCond.length) {
      if ((ruleCond(i) != '#') && (ruleCond(i) != st(i)))
        return false
    }
    true
  }

  def getPhenotypeData: String = {
    action.data.get.bits
  }

  def getConditionData: String = {
    condition.data.get.bits
  }

  def isMoreGeneralThan: ClassifierRule[Condition,Phenotype,Double] => Boolean = {cl =>
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


  def getWCCardinality: Int = {
    condition.data.get.wc.cardinality()
  }

  def report(): String = {
    var cnd: String = condition.data.get.bits
    var act: String = action.data.get.bits
    s"Rule : \nCondition: $cnd " + s"\nAction: $act" + s"\nPrediction: $prediction" + s"\nFitness: $fitness" +
      s"\nTime Stamp: $timeStamp" + s"\nNumerosity: $numerosity"
  }
}
