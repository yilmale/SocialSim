package xcs

import Constants._

trait usLCS[Condition,Action,Reward] {
  type ClassifierType = ClassifierRule[Condition,Action,Reward]
  type ClassifierPopType = ClassifierPopulation[Condition,Action,Reward]
  type PredictionArray = scala.collection.mutable.Map[Action,Option[Double]]
  type TrainingInstance = Condition
  val classifierEnsemble : ClassifierPopType
  var previousReward: Double = 0.0
  var currentReward: Double = 0.0
  var previousSituation : Condition = _

  def population : ClassifierPopType = classifierEnsemble
  def previousActionSet : List[ClassifierType] = classifierEnsemble.previousActionSet
  def currentActionSet : List[ClassifierType] = classifierEnsemble.currentActionSet
  def matchSet : List[ClassifierType] = classifierEnsemble.matchSet
  def getRewardValue: Reward => Double

  def run : Scenario[Condition,Action,Reward] => Unit = {s =>
    println("Beginning learning iterations.")
    while (s.more) {
      val situation = s.sense
      val PA = generatePredictionArray(makeMatchSet(situation,s),s)
      val act = selectAction(PA)
      generateActionSet(act)
      val r = s.execute(act)
      currentReward = getRewardValue(r)
      if (previousActionSet.nonEmpty) {
        val P = getRewardValue(r) + (discountFactor * getMax(PA))
        classifierEnsemble.updatePreviousActionSet(P)
        run_GA(previousActionSet,s.steps,previousSituation)
      }
      if (s.eop) {
        val P = getRewardValue(r)
        classifierEnsemble.updateCurrentActionSet(P)
        run_GA(currentActionSet,s.steps,situation)
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

  def generatePredictionArray: (List[ClassifierType],Scenario[Condition,Action,Reward]) => PredictionArray = { (M, s) =>
    val pa: PredictionArray = scala.collection.mutable.Map[Action, Option[Double]]()
    val fitnessSum: scala.collection.mutable.Map[Action, Double] = scala.collection.mutable.Map[Action, Double]()
    s.possibleActions foreach { a =>
      pa(a)=None
      fitnessSum(a)=0.0
    }
    M foreach { r =>
      if (pa(r.action).isEmpty)
        pa(r.action) = Some(r.prediction * r.fitness)
      else
        pa(r.action) = Some(pa(r.action).get + (r.prediction * r.fitness))
      fitnessSum(r.action) = fitnessSum(r.action) + r.fitness
    }
    s.possibleActions foreach { a =>
      if (pa(a).isDefined && (fitnessSum(a) != 0))
        pa(a) = Some(pa(a).get / fitnessSum(a))
    }
    pa
  }

  def getMax(PA: PredictionArray) : Double = {
    getMaxPrediction(PA)._2
  }

  def getMaxPrediction(PA: PredictionArray) : (Action,Double)  = {
    var maxAction : Action = PA.head._1
    var currentMax: Double = PA.head._2.get
    PA foreach {p =>
      if (p._2.get > currentMax) {
        currentMax = p._2.get
        maxAction = p._1
      }
    }
    (maxAction,currentMax)
  }


  def selectAction : PredictionArray => Action = {pa =>
    val predictions = pa.keys
    if (rng.nextDouble() < pExploration) {
      val n = rng.nextInt(predictions.size-1)
      val elm : Action = predictions.iterator.drop(n).next()
      elm
    } else getMaxPrediction(pa)._1
  }

  def clearActionSet(): Unit = {
    classifierEnsemble.clearActionSet()
  }

  def addClassifiertoActionSet(cl: ClassifierType): Unit = {
    classifierEnsemble.addClassifiertoActionSet(cl)
  }

  def generateActionSet(a: Action): Unit = {
    clearActionSet()
    matchSet foreach {m =>
      if (m.action.equals(a))
        addClassifiertoActionSet(m)
    }
  }

  def run_GA : (List[ClassifierType],Int, Condition) => Unit = {(actionSet, time,cnd) =>
    classifierEnsemble.run_GA(actionSet,time,cnd)
  }

  def report : (ClassifierType => Boolean) => List[ClassifierType] = {f =>
    val result : List[ClassifierType] = population.popSet.filter(f)
    result foreach {cl =>
      println(cl.report())
    }
    result
  }
}