package lcs
import Constants._


class UCS {
  var population : ClassifierSet = new ClassifierSet()
  var exploreIteration : Int = 0
  val MAXITERATIONS = 1000

  def run(): Unit = {
    val env : Environment = new Environment()
    println("Beginning learning iterations.")
    while (exploreIteration < MAXITERATIONS) {
      val state_phenotype  = env.sense
      runIteration(state_phenotype,exploreIteration)
      exploreIteration += 1
    }
    report((cl: Classifier) => {
      (cl.fitness > 0.8) && (cl.accuracy > 0.8)
    })
  }

  def runIteration(instance:Tuple2[Condition,Phenotype],step: Int) : Unit = {
    makeMatchSet(instance,step)
    makeCorrectSet(instance._2)
    updateSets(step)
    if (doSubsumption) doCorrectSetSubsumption()
    run_GA(step, instance._1, instance._2)
    clearSets()
  }

  def makeMatchSet(instance:Tuple2[Condition,Phenotype],step: Int): Unit = {
    population.makeMatchSet(instance,step)
  }

  def makeCorrectSet(action: Phenotype): Unit = {
    population.makeCorrectSet(action)
  }

  def updateSets(time: Int): Unit = {
    population.updateSets(time)
  }

  def doCorrectSetSubsumption(): Unit = {
    population.doCorrectSetSubsumption()
  }

  def run_GA(step: Int, c: Condition, p: Phenotype): Unit = {
    population.run_GA(step,c,p)
  }

  def clearSets(): Unit = {
    population.clearSets()
  }

  def report(f: Classifier => Boolean): Unit = {
    population.popSet.filter(f) foreach {cl =>
      println(cl.report())
    }
  }

}
