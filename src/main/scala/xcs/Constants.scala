package xcs

object Constants {

  val nu: Int = 5
  val N: Int = 1000
  val p_wc: Double = 0.1
  var rng: scala.util.Random = new scala.util.Random()
  var chi: Double = 0.8
  var upsilon: Double = 0.04
  var theta_GA: Int = 25
  var fitnessReduction: Double = 0.1
  var doGASubsumption: Boolean = true
  var theta_del: Int = 20
  var delta: Double = 0.1
  var discountFactor : Double = 0.71
  var theta_minaction : Int = 2
  var initialPrediction : Double = 0.0
  var initialError : Double = 0.0
  val initialFitness: Double = 0.0
  val pExploration : Double = 0.5
  val beta : Double = 0.2
  val alpha : Double = 0.1
  val errorThreshold : Double = 10
  var doSActionSetSubsumption: Boolean = true
  var theta_sub: Int = 20




}