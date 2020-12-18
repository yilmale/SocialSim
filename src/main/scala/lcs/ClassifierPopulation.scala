package lcs

import Constants._

trait ClassifierPopulation[Condition,Action,Reward] {
  type ClassifierType = ClassifierRule[Condition,Action,Reward]
  var popSet : List[ClassifierType] = List()
  var matchSet : List[ClassifierType] = List()
  var correctSet : List[ClassifierType] = List()
  var microPopSize : Int = 0

  def getActionData : Action => String
  def classifierCover: (Int, Condition, Action, Int) => ClassifierType
  def classifierGenerator: ClassifierType => ClassifierType
  def applyMutation(cl: ClassifierType,c: Condition, p: Action): Unit
  def applyCrossOver(cl1: ClassifierType, cl2: ClassifierType): Unit


  def addClassifierToPopulation(cl: ClassifierType) : Unit = {
    popSet = cl :: popSet
    microPopSize += 1
  }

  def makeMatchSet(dataInstance: (Condition, Action), step: Int): Unit = {
    val state = dataInstance._1
    val phenotype = dataInstance._2
    var doCovering : Boolean = true
    var setNumerositySum : Int = 0

    // --------------------------------------------------------
    // MATCHING
    // --------------------------------------------------------
    popSet foreach { cl =>
      if (cl.conditionMatch(state)) {
        matchSet = cl :: matchSet
        setNumerositySum += cl.numerosity

        //Covering Check --------------------------------------------------------
        if (cl.getPhenotypeData.equals(getActionData(phenotype)))
          doCovering = false
      }
    }

    while (doCovering) {
      val newCl = classifierCover(setNumerositySum+1,state,phenotype,step)
      addClassifierToPopulation(newCl)
      matchSet = newCl :: matchSet
      doCovering = false
    }
  }

  def makeCorrectSet(ptype: Action): Unit = {
    val action: String = getActionData(ptype)
    matchSet foreach {mcl =>
      val proposedAction : String = mcl.getPhenotypeData
      if (proposedAction.equals(action)) correctSet = mcl :: correctSet
    }
  }

  def updateSets(): Unit = {
    var matchSetNumerosity : Int = 0
    matchSet foreach {cl =>
      matchSetNumerosity = matchSetNumerosity + cl.numerosity
    }
    matchSet foreach { mcl =>
      mcl.updateExperience()
      mcl.updateMatchSetSize(matchSetNumerosity)
      mcl.updateAccuracy()
      mcl.updateFitness()
    }

    correctSet foreach {ccl =>
      ccl.updateCorrect()
    }
  }

  def clearSets(): Unit = {
    matchSet = List()
    correctSet = List()
  }

  def getIterStampAverage: Double = {
    var sumCL : Double = 0.0
    var numSum : Double = 0.0
    correctSet foreach {cl =>
      sumCL = sumCL + (cl.timeStampGA * cl.numerosity)
      numSum = numSum + cl.numerosity
    }
    sumCL/numSum
  }

  def run_GA(step: Int, c: Condition, p: Action): Unit = {
    if ((step - getIterStampAverage.toInt) > theta_GA) {
      updateTimeStamps(step)

      // --------------------------------------------------------
      // INITIALIZE OFFSPRING
      // --------------------------------------------------------

      val parent1 = selectOffSpring().get
      val parent2 = selectOffSpring().get
      val child1 = classifierGenerator(parent1)
      val child2 = classifierGenerator(parent2)

      // --------------------------------------------------------
      // CROSSOVER OPERATOR
      // --------------------------------------------------------

      if (rng.nextDouble() < chi) {
        applyCrossOver(child1, child2)
        child1.setAccuracy((child1.accuracy + child2.accuracy) / 2.0)
        child1.setFitness(fitnessReduction * (child1.fitness + child2.fitness) / 2.0)
        child2.setAccuracy(child1.accuracy)
        child2.setFitness(child1.fitness)
      }

      // --------------------------------------------------------
      // MUTATION OPERATOR
      // --------------------------------------------------------

      val childList: List[ClassifierType] = List(child1, child2)

      childList foreach { ch =>
        applyMutation(ch, c, p)
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

  def removeFromMatchSet(cl: ClassifierType): Unit = {
    matchSet = matchSet.filter(_ != cl)
  }

  def removeFromCorrectSet(cl: ClassifierType): Unit = {
    correctSet = correctSet.filter(_ != cl)
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


  def selectOffSpring(): Option[ClassifierType] = {
    var fitnessSum: Double = 0.0
    var result: Option[ClassifierType] = None
    correctSet foreach {cl =>
      fitnessSum = fitnessSum + cl.fitness
    }
    val choicePoint = rng.nextDouble() * fitnessSum
    fitnessSum = 0.0
    var found : Boolean = false
    val setIter = correctSet.iterator
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

  def doCorrectSetSubsumption(): Unit = {
    var subsumer : Option[ClassifierType] = None
    correctSet foreach {cl =>
      if (cl.isSubsumer) {
        if (subsumer.isEmpty ||
          (subsumer.isDefined && cl.isMoreGeneralThan(subsumer.get)))
          subsumer = Some(cl)
      }
    }

    if (subsumer.isDefined) {
      val subsumerCL : ClassifierType = subsumer.get
      correctSet foreach {cl =>
        if (subsumerCL.isMoreGeneralThan(cl)) {
          subsumerCL.updateNumerosity(cl.numerosity)
          removeMacroClassifier(cl)
          removeFromMatchSet(cl)
          removeFromCorrectSet(cl)
        }
      }
    }
  }

  def updateTimeStamps(step: Int): Unit = {
    correctSet foreach {cl =>
      cl.updateTimeStamp(step)
    }
  }

}