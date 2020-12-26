package xcs

import Constants._

trait usLCS[Condition,Action,Reward] {
  type ClassifierType = ClassifierRule[Condition,Action,Reward]
  type ClassifierPopType = ClassifierPopulation[Condition,Action,Reward]
  type PredictionArray = scala.collection.mutable.Map[String,Option[Double]]
  type TrainingInstance = Condition
  val classifierEnsemble : ClassifierPopType
  var previousReward: Double = 0.0
  var currentReward: Double = 0.0
  var previousSituation : Condition = _
  var scenario : Option[Scenario[Condition,Action,Reward]] = None

  def population : ClassifierPopType = classifierEnsemble
  def previousActionSet : List[ClassifierType] = classifierEnsemble.previousActionSet
  def currentActionSet : List[ClassifierType] = classifierEnsemble.currentActionSet
  def matchSet : List[ClassifierType] = classifierEnsemble.matchSet

  def run : Scenario[Condition,Action,Reward] => Unit = {s =>
    println("Beginning learning iterations.")
    scenario = Some(s)
    while (s.more) {
      val situation = s.sense
      val PA = generatePredictionArray(makeMatchSet(situation._1))
      val act = selectAction(PA)
      generateActionSet(act)
      val r = s.execute(act)
      currentReward = s.getRewardValue(r)
      if (previousActionSet.nonEmpty) {
        val P = previousReward + (discountFactor * getMax(PA))
        classifierEnsemble.updatePreviousActionSet(P)
        run_GA(previousActionSet,s.steps,previousSituation)
      }
      if (s.eop) {
        val P = currentReward
        classifierEnsemble.updateCurrentActionSet(P)
        run_GA(currentActionSet,s.steps,situation._1)
        classifierEnsemble.previousActionSet = List()
      }
      else {
        classifierEnsemble.previousActionSet = currentActionSet
        previousReward = currentReward
        previousSituation = situation._1
      }
    }
  }


  def makeMatchSet : TrainingInstance => List[ClassifierType] = {instance =>
    classifierEnsemble.makeMatchSet(instance,scenario.get)
    classifierEnsemble.matchSet
  }

  def generatePredictionArray: (List[ClassifierType]) => PredictionArray = { M =>
    val pa: PredictionArray = scala.collection.mutable.Map[String, Option[Double]]()
    val fitnessSum: scala.collection.mutable.Map[String, Double] = scala.collection.mutable.Map[String, Double]()
    val s = scenario.get
    s.possibleActions foreach { a =>
      val act = s.getActionData(a)
      pa(act)=None
      fitnessSum(act)=0.0
    }
    M foreach { r =>
      val act = s.getActionData(r.action)
      if (pa(act).isEmpty)
        pa(act) = Some(r.prediction * r.fitness)
      else
        pa(act) = Some(pa(act).get + (r.prediction * r.fitness))
      fitnessSum(act) = fitnessSum(act) + r.fitness
    }
    s.possibleActions foreach { a =>
      val act = s.getActionData(a)
      if (pa(act).isDefined && (fitnessSum(act) != 0.0))
        pa(act) = Some(pa(act).get / fitnessSum(act))
    }

    pa
  }

  def getMax(PA: PredictionArray) : Double = {
    getMaxPrediction(PA)._2
  }

  def getMaxPrediction(PA: PredictionArray) : (String,Double)  = {
    var activePredictions = scala.collection.mutable.Map[String,Double]()
    PA foreach {p =>
      if (p._2.isDefined) {
        activePredictions += (p._1 -> p._2.get)
      }
    }
    var maxAction : String = activePredictions.head._1
    var currentMax: Double = activePredictions.head._2
    activePredictions foreach {p =>
      if (p._2 > currentMax) {
          currentMax = p._2
          maxAction = p._1
        }
      }
    (maxAction,currentMax)
  }


  def selectAction : PredictionArray => String = {pa =>
    var predictions : List[String]= List()
    var selectedAction : String = ""
    pa foreach {p =>
      if (p._2.isDefined) predictions = p._1 :: predictions
    }
    val candidates: Array[String] = predictions.toArray
    val n = rng.nextInt(predictions.size)
    if (rng.nextDouble() < pExploration)
      selectedAction = candidates(n)
    else
      selectedAction = getMaxPrediction(pa)._1
    selectedAction
  }

  def clearActionSet(): Unit = {
    classifierEnsemble.clearActionSet()
  }

  def addClassifiertoActionSet(cl: ClassifierType): Unit = {
    classifierEnsemble.addClassifiertoActionSet(cl)
  }

  def generateActionSet(a: String): Unit = {
    clearActionSet()
    val s = scenario.get
    matchSet foreach {m =>
      if (s.getActionData(m.action).equals(a))
        addClassifiertoActionSet(m)
    }
  }

  def run_GA : (List[ClassifierType],Int,Condition) => Unit = {(actionSet, time,cnd) =>
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