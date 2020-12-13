package lcs

import Constants._

trait sLCS[Condition,Action,Reward] {
  type ClassifierType = ClassifierRule[Condition,Action,Reward]
  type ClassifierPopType = ClassifierPopulation[Condition,Action,Reward]
  val classifierEnsemble : ClassifierPopType
  type TrainingInstance = Tuple2[Condition,Action]
  def population : ClassifierPopType = classifierEnsemble
  def run : Scenario[Condition,Action,Reward] => Unit = {s =>
    println("Beginning learning iterations.")
    while (s.more) {
      val situation = s.sense
      makeMatchSet(situation,s.steps)
      makeCorrectSet(situation._2)
      s.execute(situation._2)
      updateSets(s.steps)
      if (doSubsumption) doCorrectSetSubsumption()
      run_GA(s.steps, situation._1, situation._2)
      clearSets()
    }
  }

  def runIteration : (TrainingInstance, Int) => Unit = {(instance,step) =>
    makeMatchSet(instance,step)
    makeCorrectSet(instance._2)
    updateSets(step)
    if (doSubsumption) doCorrectSetSubsumption()
    run_GA(step, instance._1, instance._2)
    clearSets()
  }

  def makeMatchSet : (TrainingInstance, Int) => List[ClassifierType] = {(instance,step) =>
    classifierEnsemble.makeMatchSet(instance,step)
    classifierEnsemble.matchSet
  }

  def makeCorrectSet : Action => List[ClassifierType] = {a =>
    classifierEnsemble.makeCorrectSet(a)
    classifierEnsemble.correctSet
  }

  def updateSets(step: Int) : ClassifierPopType = {
    classifierEnsemble.updateSets(step)
    classifierEnsemble
  }

  def clearSets() : ClassifierPopType = {
    classifierEnsemble.clearSets()
    classifierEnsemble
  }


  def doCorrectSetSubsumption() : ClassifierPopType = {
    classifierEnsemble.doCorrectSetSubsumption()
    classifierEnsemble
  }

  def run_GA : (Int, Condition, Action) => ClassifierPopType = {(step, c,a) =>
    classifierEnsemble.run_GA(step,c,a)
    classifierEnsemble

  }
  def report : (ClassifierType => Boolean) => List[ClassifierType] = {f =>
    val result : List[ClassifierType] = population.popSet.filter(f)
    result foreach {cl =>
      println(cl.report())
    }
    result
  }
}