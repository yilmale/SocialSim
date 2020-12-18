package lcs

import Constants._

trait sLCS[Condition,Action,Reward] {
  type ClassifierType = ClassifierRule[Condition,Action,Reward]
  type ClassifierPopType = ClassifierPopulation[Condition,Action,Reward]
  val classifierEnsemble : ClassifierPopType
  type TrainingInstance = (Condition, Action)
  def action : TrainingInstance => Action = {instance => instance._2}
  def condition: TrainingInstance => Condition = {instance => instance._1}
  def population : ClassifierPopType = classifierEnsemble
  def run : Scenario[Condition,Action,Reward] => Unit = {s =>
    println("Beginning learning iterations.")
    while (s.more) {
      val situation = s.sense
      makeMatchSet(situation,s.steps)
      makeCorrectSet(action(situation))
      s.execute(action(situation))
      updateSets()
      if (doSubsumption) doCorrectSetSubsumption()
      run_GA(s.steps, condition(situation), action(situation))
      clearSets()
    }
  }

  def makeMatchSet : (TrainingInstance, Int) => List[ClassifierType] = {(instance,step) =>
    classifierEnsemble.makeMatchSet(instance,step)
    classifierEnsemble.matchSet
  }

  def makeCorrectSet : Action => List[ClassifierType] = {a =>
    classifierEnsemble.makeCorrectSet(a)
    classifierEnsemble.correctSet
  }

  def updateSets() : ClassifierPopType = {
    classifierEnsemble.updateSets()
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