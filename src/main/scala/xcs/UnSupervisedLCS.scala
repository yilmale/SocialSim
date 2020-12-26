package xcs

import xcs.Constants.discountFactor

class UnSupervisedLCS extends usLCS[Condition,Phenotype,Double]  {
  val classifierEnsemble = new ClassifierSet()

  override def run: Scenario[Condition, Phenotype, Double] => Unit = super.run
}

class UnSupervisedLCSDebug extends usLCS[Condition,Phenotype,Double] with Reporter {
  val classifierEnsemble = new ClassifierSet()

  override def run: Scenario[Condition, Phenotype, Double] => Unit = {s =>
    println("Beginning learning iterations.")
    scenario = Some(s)
    while (s.more) {
      val situation = s.sense
      val PA = generatePredictionArray(makeMatchSet(situation._1))
      println("Time: " + s.steps + " " + reportCondition(situation._1) + " " + reportAction(situation._2))
      println("MatchSet:")
      reportSet(classifierEnsemble.matchSet)
      println("Prediction Array: ")
      reportPredictionArray(PA)
      val act = selectAction(PA)
      println("Selected action: " + act)
      generateActionSet(act)
      println("CurrentActionSet: ")
      reportSet(classifierEnsemble.currentActionSet)
      val r = s.execute(act)
      currentReward = s.getRewardValue(r)
      println("Reward: " + currentReward)
      println("--------")
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
}
