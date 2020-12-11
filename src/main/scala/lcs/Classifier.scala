package lcs
import Constants._


class Classifier(setSize: Int, situation: Condition, act: Phenotype, time: Int) {
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

  def updateExperience(): Unit = {
    matchCount = matchCount + 1
  }

  def updateCorrect(): Unit = {
    correctCount = correctCount + 1
  }

  def updateAccuracy(): Unit = {
    accuracy = correctCount.toDouble / matchCount.toDouble
  }

  def updateFitness(): Unit = {
    fitness = Math.pow(accuracy,nu)
  }

  def updateMatchSetSize(numerositySum: Int): Unit = {
    if (matchCount < 1.0 / beta)
      avgMatchSize = avgMatchSize + (numerositySum - avgMatchSize)/matchCount
    else avgMatchSize = avgMatchSize + (beta * (numerositySum - avgMatchSize)).toInt
  }

  def updateNumerosity(num: Int): Unit = {
    numerosity += num
  }

  def setAccuracy(acc: Double): Unit = {
    accuracy = acc
  }

  def setFitness(fit: Double): Unit = {
    fitness = fit
  }

  def updateTimeStamp(step: Int): Unit = {
    timeStampGA = step
  }

  def deletionVote(meanFitness: Double): Double = {
    var vote : Double = 0.0
    if ((fitness/numerosity > delta ) || (matchCount > theta_del))
      vote = avgMatchSize * numerosity
    else if (fitness == 0) vote = avgMatchSize * numerosity * meanFitness / (initialFitness/numerosity)
    else vote = avgMatchSize * numerosity * meanFitness / (fitness/numerosity)
    vote
  }

  def isSubsumer: Boolean = {
    var canSubsume : Boolean = false
    if ((matchCount > theta_sub) && (accuracy > acc_sub))
      canSubsume = true
    canSubsume
  }

  def isMoreGeneralThan(cl: Classifier): Boolean = {
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

  def subsumes(cl: Classifier): Boolean = {
    var doesSubsume : Boolean = false
    if (action.data.get.bits.equals(cl.action.data.get.bits)) {
      if (isSubsumer && isMoreGeneralThan(cl)) doesSubsume = true
    }
    doesSubsume
  }

  def report(): String = {
    var cnd: String = condition.data.get.bits
    var act: String = action.data.get.bits
    s"Rule : \nCondition: $cnd " + s"\nAction: $act" + s"\nAccuracy: $accuracy" + s"\nFitness: $fitness" +
      s"\nMatch Count: $matchCount" + s"\nCorrect Count: $correctCount" + s"\nTime Stamp: $iniTimeStamp" +
      s"\nNumerosity: $numerosity"
  }

}


object ClassifierCover {
  def apply(setSize: Int, situation: Condition, act: Phenotype, time: Int): Classifier = {
    var cnd : Condition = new Condition()
    var context : String = situation.data.get.bits
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
    var gen = new Classifier(1,
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


