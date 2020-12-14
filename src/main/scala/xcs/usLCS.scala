package xcs

import Constants._

trait usLCS[Condition,Action,Reward] {
  type ClassifierType = ClassifierRule[Condition,Action,Reward]
  type ClassifierPopType = ClassifierPopulation[Condition,Action,Reward]
  type PredictionArray = scala.collection.mutable.Map[Action,Double]
  type TrainingInstance = Condition
  val classifierEnsemble : ClassifierPopType
  var previousReward: Double = 0.0
  var currentReward: Double = 0.0
  var previousSituation : Condition = _

  def population : ClassifierPopType = classifierEnsemble
  def previousActionSet : List[ClassifierType] = classifierEnsemble.previousActionSet
  def currentActionSet : List[ClassifierType] = classifierEnsemble.currentActionSet
  def generatePredictionArray: List[ClassifierType] => PredictionArray
  def selectAction : PredictionArray => Action
  def generateActionSet(a: Action): Unit
  def getRewardValue: Reward => Double
  def getMaxPrediction: PredictionArray => Double
  def updateSet(A: List[ClassifierType],P: Double) : Unit

  def run : Scenario[Condition,Action,Reward] => Unit = {s =>
    println("Beginning learning iterations.")
    while (s.more) {
      val situation = s.sense
      val M = makeMatchSet(situation,s)
      val PA = generatePredictionArray(M)
      val act = selectAction(PA)
      generateActionSet(act)
      val r = s.execute(act)
      currentReward = getRewardValue(r)
      if (previousActionSet.nonEmpty) {
          val P = getRewardValue(r) + (discountFactor * getMaxPrediction(PA))
          updateSet(previousActionSet,P)
          run_GA(s.steps,previousSituation)
      }
      if (s.eop) {
        val P = getRewardValue(r)
        updateSet(currentActionSet,P)
        classifierEnsemble.previousActionSet = List()
      }
      else {
        classifierEnsemble.previousActionSet = currentActionSet
        previousReward = currentReward
        previousSituation = situation
      }
    }
  }

  def makeMatchSet : (TrainingInstance, Scenario[Condition,Action,Reward]) =>
    List[ClassifierType] = {(instance,s) =>
    classifierEnsemble.makeMatchSet(instance,s)
    classifierEnsemble.matchSet
  }


  def run_GA : (Int, Condition) => ClassifierPopType

  def report : (ClassifierType => Boolean) => List[ClassifierType] = {f =>
    val result : List[ClassifierType] = population.popSet.filter(f)
    result foreach {cl =>
      println(cl.report())
    }
    result
  }
}