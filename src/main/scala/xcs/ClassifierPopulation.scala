package xcs

import Constants._

trait ClassifierPopulation[Condition,Action,Reward] {
  type ClassifierType = ClassifierRule[Condition,Action,Reward]
  var popSet : List[ClassifierType] = List()
  var matchSet : List[ClassifierType] = List()
  var currentActionSet: List[ClassifierType] = List()
  var previousActionSet: List[ClassifierType] = List()

  def getActionData : Action => String
  def generateAction : String => Action
  def initializeClassifier : (Condition,Int) => ClassifierType
  def classifierGenerator(cl: ClassifierType) : ClassifierType
  def applyMutation(cl: ClassifierType,c: Condition): Unit
  def applyCrossOver(cl1: ClassifierType, cl2: ClassifierType): Unit

  def possibleActions : Scenario[Condition,Action,Reward] => scala.collection.immutable.Set[String] = {s =>
    s.possibleActions.map(a => getActionData(a)).toSet}

  def availableActions : scala.collection.immutable.Set[String] = {
    var actionSet: scala.collection.immutable.Set[String] = Set()
    matchSet foreach { mr =>
      actionSet = actionSet + getActionData(mr.action)
    }
    actionSet
  }

  def clearActionSet(): Unit = {
    currentActionSet = List()
  }

  def selectRandomAction : Set[String] => Action = {s =>
    val elems = s.toArray
    generateAction(elems(rng.nextInt(elems.length)))
  }

  def getActionCount : Int = {
    var actionSet: Set[String] = Set()
    matchSet foreach { mr =>
      actionSet = actionSet + getActionData(mr.action)
    }
    actionSet.size
  }

  def classifierCover: (Condition, Scenario[Condition,Action,Reward]) => ClassifierType = {(c,s) =>
    val cl = initializeClassifier(c,s.steps)
    cl.action = selectRandomAction(possibleActions(s).diff(availableActions))
    cl.prediction = initialPrediction
    cl.error = initialError
    cl.fitness = initialFitness
    cl.experience = 0
    cl.timeStamp = s.steps
    cl.actionSetSize = 1
    cl.numerosity = 1
    cl
  }

  def addClassifierToPopulation(cl: ClassifierType) : Unit = {
    popSet = cl :: popSet
  }

  def addClassifiertoActionSet(cl: ClassifierType): Unit = {
    currentActionSet = cl :: currentActionSet
  }

  def makeMatchSet(dataInstance: Condition, scenario: Scenario[Condition,Action,Reward]):
  List[ClassifierType] = {
    val state = dataInstance
    matchSet = List()
    while (matchSet.isEmpty) {
      popSet foreach { cl =>
        if (cl.conditionMatch(state))
          matchSet = cl :: matchSet
      }
      if (getActionCount < theta_minaction) {
          addClassifierToPopulation(classifierCover(state, scenario))
          deleteFromPopulation()
          matchSet = List()
        }
      }
    matchSet
  }


  def clearSets(): Unit = {
    matchSet = List()
  }

  def getIterStampAverage(actionSet: List[ClassifierType]): Double = {
    var sumCL : Double = 0.0
    var numSum : Double = 0.0
    actionSet foreach {cl =>
      sumCL = sumCL + (cl.timeStampGA * cl.numerosity)
      numSum = numSum + cl.numerosity
    }
    sumCL/numSum
  }

  def run_GA(actionSet: List[ClassifierType],step: Int, c: Condition): Unit = {
    if ((step - getIterStampAverage(actionSet).toInt) > theta_GA) {
      updateTimeStamps(actionSet,step)

      // --------------------------------------------------------
      // INITIALIZE OFFSPRING
      // --------------------------------------------------------

      val parent1 = selectOffSpring(actionSet).get
      val parent2 = selectOffSpring(actionSet).get
      val child1 = classifierGenerator(parent1)
      val child2 = classifierGenerator(parent2)

      // --------------------------------------------------------
      // CROSSOVER OPERATOR
      // --------------------------------------------------------

      if (rng.nextDouble() < chi) {
        applyCrossOver(child1, child2)
        child1.setPrediction((parent1.prediction + parent2.prediction) / 2.0)
        child1.setError((parent1.prediction + parent2.prediction) / 2.0)
        child1.setFitness(fitnessReduction * (parent1.fitness + parent2.fitness) / 2.0)
        child1.setPrediction(child1.prediction)
        child2.setError(child1.error)
        child2.setFitness(child1.fitness)
      }

      // --------------------------------------------------------
      // MUTATION OPERATOR
      // --------------------------------------------------------

      val childList: List[ClassifierType] = List(child1, child2)

      childList foreach { ch =>
        applyMutation(ch, c)
        if (doGASubsumption) {
          if (parent1.subsumes(ch))
            parent1.numerosity = parent1.numerosity + 1
          else if (parent2.subsumes(ch))
            parent2.numerosity = parent2.numerosity + 1
          else insertInPopulation(ch)
        }
        else insertInPopulation(ch)
      }
      deleteFromPopulation()
    }
  }

  def insertInPopulation(ch: ClassifierType): Unit = {
    val setIter = popSet.iterator
    var found : Boolean = false

    while (setIter.hasNext && !found) {
      val cl = setIter.next()
      if  (cl.getConditionData.equals(ch.getConditionData) &&
        cl.getPhenotypeData.equals(ch.getPhenotypeData)) {
        found = true
        cl.numerosity = cl.numerosity + 1
      }
    }

    if (!found) popSet = ch :: popSet
  }

  def numerositySum : Int = {
    var numSum: Int = 0
    popSet foreach {cl =>
      numSum = numSum + cl.numerosity
    }
    numSum
  }

  def avgPopulationFitness: Double = {
    var avgFitness : Double = 0
    var cumFitness : Double = 0
    popSet foreach {cl =>
      cumFitness = cumFitness + cl.fitness
    }
    avgFitness = cumFitness /numerositySum
    avgFitness
  }

  def removeMacroClassifier(cl: ClassifierType): Unit = {
    popSet = popSet.filter(_ != cl)
  }

  def removeFromActionSet(cl: ClassifierType, actionSetId: Int): Unit = {
   if (actionSetId == 0) previousActionSet = previousActionSet.filter(_ != cl)
   else currentActionSet = currentActionSet.filter(_ != cl)
  }


  def deleteFromPopulation(): Unit = {
    if (numerositySum > N) {
      val avgFitness = avgPopulationFitness
      var voteSum :Double = 0.0
      popSet foreach {cl =>
        voteSum = voteSum + cl.deletionVote(avgFitness)
      }
      val choicePoint = rng.nextDouble()* voteSum
      voteSum = 0
      val setIter = popSet.iterator
      var found : Boolean = false
      while (setIter.hasNext && (!found)) {
        val c: ClassifierType = setIter.next()
        voteSum = voteSum + c.deletionVote(avgFitness)
        if (voteSum > choicePoint) {
          if (c.numerosity > 1) c.numerosity = c.numerosity - 1
          else removeMacroClassifier(c)
          found = true
        }
      }
    }
  }


  def selectOffSpring(actionSet: List[ClassifierType]): Option[ClassifierType] = {
    var fitnessSum: Double = 0.0
    var result: Option[ClassifierType] = None
    actionSet foreach {cl =>
      fitnessSum = fitnessSum + cl.fitness
    }
    val choicePoint = rng.nextDouble() * fitnessSum
    fitnessSum = 0.0
    var found : Boolean = false
    val setIter = actionSet.iterator
    while ((!found) && setIter.hasNext) {
      val cl = setIter.next()
      fitnessSum = fitnessSum + cl.fitness
      if (fitnessSum >=  choicePoint) {
        found = true
        result = Some(cl)
      }
    }
    result
  }

  def doActionSetSubsumption(actionSetId: Int): Unit = {
    var actionSet : Option[List[ClassifierType]] = None
    if (actionSetId == 0) actionSet = Some(previousActionSet)
    else actionSet = Some(currentActionSet)
    var subsumer : Option[ClassifierType] = None
    var removeList : List[ClassifierType] = List()
    actionSet.get foreach {cl =>
      if (cl.isSubsumer) {
        if (subsumer.isEmpty ||
          (subsumer.isDefined && cl.isMoreGeneralThan(subsumer.get)))
          subsumer = Some(cl)
      }
    }

    if (subsumer.isDefined) {
      val subsumerCL : ClassifierType = subsumer.get
      actionSet.get foreach {cl =>
        if (subsumerCL.isMoreGeneralThan(cl)) {
          subsumerCL.updateNumerosity(cl.numerosity)
          removeList = cl :: removeList
        }
      }
      removeList foreach {cl =>
        removeMacroClassifier(cl)
        removeFromActionSet(cl,actionSetId)
      }
    }
  }

  def updateFitness(actionSet: List[ClassifierType]): Unit = {
    var accuracySum : Double = 0.0
    var accuracyVector : scala.collection.mutable.Map[ClassifierType,Double] =
      scala.collection.mutable.Map[ClassifierType,Double]()
    actionSet foreach {cl=>
      if (cl.error < errorThreshold)
        accuracyVector += (cl -> 1.0)
      else accuracyVector += (cl -> alpha*Math.pow(cl.error/errorThreshold,-nu))
      accuracySum = accuracySum + (accuracyVector(cl)*cl.numerosity)
    }
    actionSet foreach {cl=>
      cl.fitness = cl.fitness + (beta*((accuracyVector(cl) * cl.numerosity/accuracySum)-cl.fitness))
    }

  }

  def updatePreviousActionSet(P: Double) : Unit = {
    var actionSetNumerosity : Int = 0
    previousActionSet foreach {cl =>
      actionSetNumerosity = actionSetNumerosity + cl.numerosity
    }
    previousActionSet foreach {cl=>
      cl.updateExperience()
      cl.updatePrediction(P)
      cl.updatePredictionError(P)
      cl.updateActionSetSizeEstimate(P,actionSetNumerosity)
    }
    updateFitness(previousActionSet)
    if (doSActionSetSubsumption) doActionSetSubsumption(0)

  }

  def updateCurrentActionSet(P: Double) : Unit = {
    var actionSetNumerosity : Int = 0
    currentActionSet foreach {cl =>
      actionSetNumerosity = actionSetNumerosity + cl.numerosity
    }
    currentActionSet foreach {cl=>
      cl.updateExperience()
      cl.updatePrediction(P)
      cl.updatePredictionError(P)
      cl.updateActionSetSizeEstimate(P,actionSetNumerosity)
    }
    updateFitness(currentActionSet)
    if (doSActionSetSubsumption) doActionSetSubsumption(1)

  }



  def updateTimeStamps(actionSet: List[ClassifierType],step: Int): Unit = {
    actionSet foreach {cl =>
      cl.updateTimeStamp(step)
    }
  }

}