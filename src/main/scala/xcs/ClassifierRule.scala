package xcs

import Constants._

trait ClassifierRule[Condition,Action,Reward] {
  var condition: Condition
  var action: Action
  var prediction : Double
  var fitness: Double
  var error: Double
  var experience : Int
  var timeStamp : Int
  var actionSetSize : Double

  var accuracy: Double
  var timeStampGA: Int
  var numerosity : Int


  def conditionMatch(c: Condition): Boolean
  def report(): String
  def getPhenotypeData: String
  def getConditionData: String
  def getWCCardinality: Int
  def isMoreGeneralThan: ClassifierRule[Condition,Action,Reward] => Boolean

  def updateExperience(): Unit = {
    experience = experience + 1
  }

  def updatePrediction(P: Double): Unit = {
    if (experience < 1.0/beta)
      prediction = prediction + ((P - prediction)/experience)
    else prediction = prediction + (beta * (P - prediction))
  }

  def updatePredictionError(P: Double): Unit = {
    if (experience < 1.0/beta)
      error = error + ((Math.abs(P - prediction) - error)/experience)
    else error = error + (beta * (Math.abs(P - prediction) - error))
  }

  def updateActionSetSizeEstimate(P: Double, numerositySum: Int): Unit = {
    if (experience < 1.0/beta)
      actionSetSize = actionSetSize + ((numerositySum - numerosity)/experience)
    else actionSetSize = actionSetSize + (beta * (numerositySum - numerosity))
  }


  def updateNumerosity(num: Int): Unit = {
    numerosity += num
  }

  def setPrediction(p: Double): Unit = {
    prediction = p
  }

  def setError(e: Double): Unit = {
    error = e
  }

  def setFitness(fit: Double): Unit = {
    fitness = fit
  }

  def updateTimeStamp(step: Int): Unit = {
    timeStampGA = step
  }

  def deletionVote(meanFitness: Double): Double = {
    var vote : Double = 0.0
    if ((fitness/numerosity >= delta * meanFitness ) || (experience < theta_del))
      vote = actionSetSize * numerosity
    else if (fitness == 0) vote = actionSetSize * numerosity * meanFitness / (initialFitness/numerosity)
    else vote = actionSetSize * numerosity * meanFitness / (fitness/numerosity)
    vote
  }

  def isSubsumer: Boolean = {
    var canSubsume : Boolean = false
    if ((experience > theta_sub) && (error < errorThreshold))
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