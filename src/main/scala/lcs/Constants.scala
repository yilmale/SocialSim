package lcs

object Constants {
  val initialFitness : Double = 0.01
  val nu : Int = 3
  val N : Int = 1000
  val p_wc : Double = 0.33
  var rng : scala.util.Random = new scala.util.Random()
  var chi : Double = 0.8
  var upsilon : Double = 0.04
  var theta_GA : Int = 25
  var fitnessReduction : Double = 0.1
  var doGASubsumption : Boolean = true
  var beta : Double = 0.2
  var theta_del : Int =20
  var delta : Double = 0.1
  var theta_sub : Int = 20
  var acc_sub : Double = 0.99
  var doSubsumption : Boolean = true
  var supervised : Boolean = true
}
