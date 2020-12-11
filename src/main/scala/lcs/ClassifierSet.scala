package lcs

import Constants._



class ClassifierSet {
  var popSet : List[Classifier] = List()
  var matchSet : List[Classifier] = List()
  var correctSet : List[Classifier] = List()
  var microPopSize : Int = 0

  def makeMatchSet(dataInstance: Tuple2[Condition,Phenotype], step: Int): Unit = {
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
        if (cl.action.data.get.bits.equals(phenotype.data.get.bits))
          doCovering = false
      }
    }

    while (doCovering) {
      val newCl = ClassifierCover(setNumerositySum+1,state,phenotype,step)
      addClassifierToPopulation(newCl)
      matchSet = newCl :: matchSet
      doCovering = false
    }
  }

  def addClassifierToPopulation(cl: Classifier): Unit = {
    popSet = cl :: popSet
    microPopSize += 1
  }

  def makeCorrectSet(ptype: Phenotype): Unit = {
    val action  : String = ptype.data.get.bits
    matchSet foreach {mcl =>
      val proposedAction : String = mcl.action.data.get.bits
      if (proposedAction.equals(action)) correctSet = mcl :: correctSet
    }
  }

  def updateSets(step: Int): Unit = {
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

  def run_GA(step: Int, c: Condition, p: Phenotype): Unit = {
    if ((step - getIterStampAverage.toInt) > theta_GA) {
      updateTimeStamps(step)

      // --------------------------------------------------------
      // INITIALIZE OFFSPRING
      // --------------------------------------------------------

      var parent1 = selectOffSpring().get
      var parent2 = selectOffSpring().get
      var child1 = ClassifierGen(parent1)
      var child2 = ClassifierGen(parent2)

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

      val childList: List[Classifier] = List(child1, child2)

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

  def applyMutation(cl: Classifier,c: Condition, p: Phenotype): Unit = {
    var sb1 : StringBuilder = new StringBuilder("")
    val cnd : String = cl.condition.data.get.bits
    val env : String = c.data.get.bits
    val size : Int = cl.condition.data.get.bits.length
    var i : Int = 0
    do {
      if (rng.nextDouble() < upsilon) {
        if (cnd(i) == '#')
          sb1 = sb1 += env(i)
        else
          sb1 = sb1 += '#'
      }
      else sb1 = sb1 += cnd(i)
      i = i + 1
    } while (i < size)
    cl.condition = Condition(sb1.toString())
    if (rng.nextDouble() < 0.5) {
      cl.action = Phenotype(BitString.randomBitString(cl.action.data.get.bits.length).bits)
    }
  }


  def applyCrossOver(cl1: Classifier, cl2: Classifier): Unit = {
    val size : Int = cl1.condition.data.get.bits.length
    var x :Int = (rng.nextDouble() * size).toInt
    var y : Int = (rng.nextDouble() * size).toInt

    if(x >y) {
      val temp = y
      y = x
      x = temp
    }

    var i = 0
    val str1 : String = cl1.condition.data.get.bits
    val str2 : String = cl2.condition.data.get.bits

    var sb1 : StringBuilder = new StringBuilder("")
    var sb2 : StringBuilder = new StringBuilder("")
    do {
      if ((x <= i) && (i < y)) {
        sb1 = sb1 += str2(i)
        sb2 = sb2 += str1(i)
      }
      else {
        sb1 = sb1 += str1(i)
        sb2 = sb2 += str2(i)
      }
      i = i+1
    } while (i < size)

    cl1.condition = Condition(sb1.toString())
    cl2.condition = Condition(sb2.toString())
  }

  def insertInPopulation(ch: Classifier): Unit = {
    var setIter = popSet.iterator
    var found : Boolean = false

    while (setIter.hasNext && !found) {
      val cl = setIter.next()
      if  ((cl.condition.data.get.bits.equals(ch.condition.data.get.bits)) &&
            (cl.action.data.get.bits.equals(ch.action.data.get.bits))) {
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

  def removeMacroClassifier(cl: Classifier): Unit = {
    popSet = popSet.filter(_ != cl)
  }

  def removeFromMatchSet(cl: Classifier): Unit = {
    matchSet = matchSet.filter(_ != cl)
  }

  def removeFromCorrectSet(cl: Classifier): Unit = {
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
      var setIter = popSet.iterator
      var found : Boolean = false
      while ((setIter.hasNext) && (!found)) {
        var c: Classifier = setIter.next()
        voteSum = voteSum + c.deletionVote(avgFitness)
        if (voteSum > choicePoint) {
          if (c.numerosity > 1) c.numerosity = c.numerosity - 1
          else removeMacroClassifier(c)
        }
      }
    }
  }


  def selectOffSpring(): Option[Classifier] = {
    var fitnessSum: Double = 0.0
    var result: Option[Classifier] = None
    correctSet foreach {cl =>
      fitnessSum = fitnessSum + cl.fitness
    }
    val choicePoint = rng.nextDouble() * fitnessSum
    fitnessSum = 0.0
    var found : Boolean = false
    val setIter = correctSet.iterator
    while ((!found) && (setIter.hasNext)) {
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
    var subsumer : Option[Classifier] = None
    correctSet foreach {cl =>
      if (cl.isSubsumer) {
        if ((subsumer.isEmpty) ||
           ((subsumer.isDefined) && (cl.isMoreGeneralThan(subsumer.get))))
                   subsumer = Some(cl)
      }
    }

    if (subsumer.isDefined) {
      var subsumerCL : Classifier = subsumer.get
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


