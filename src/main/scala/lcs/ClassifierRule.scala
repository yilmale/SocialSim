package lcs

import Constants._

trait ClassifierRule[Condition,Action,Reward] {
  var condition: Condition
  var action: Action
  var fitness: Double
  var accuracy: Double
  var matchCount: Int
  var correctCount: Int
  var iniTimeStamp: Int
  var timeStampGA: Int
  var numerosity : Int
  var avgMatchSize : Int

  def conditionMatch(c: Condition): Boolean
  def report(): String
  def getPhenotypeData: String
  def getConditionData: String
  def getWCCardinality: Int
  def isMoreGeneralThan: ClassifierRule[Condition,Action,Reward] => Boolean

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

  def updateMatchSetSize(numerositySum: Int):  Unit = {
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
    if ((fitness/numerosity >= delta * meanFitness ) || (matchCount < theta_del))
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

  def subsumes: ClassifierRule[Condition,Action,Reward] => Boolean = {cl =>
    var doesSubsume : Boolean = false
    if (getPhenotypeData.equals(cl.getPhenotypeData)) {
      if (isSubsumer && isMoreGeneralThan(cl)) doesSubsume = true
    }
    doesSubsume
  }
}