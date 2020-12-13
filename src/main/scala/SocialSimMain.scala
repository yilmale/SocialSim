import PythonUtilities.{executeModule, interp, loadModule}
import breeze.linalg._
import breeze.linalg.operators.BitVectorOps
import lcs._
import BitString._
import lcs.Constants.beta

import scala.io.Source





object SocialSimMain extends App {

  //loadModule("/Users/yilmaz/IdeaProjects/SocialSim/src/FCM.py",interp)

  //var result1 = executeModule("performExperiments")
  //println(result1)

  var ucs : UCS = new UCS()
  var mux : Scenario[Condition,Phenotype,Boolean] = new MultiplexProblem(2000)
  ucs.run(mux)
  ucs.report(cl => (cl.fitness > 0.8) && (cl.accuracy > 0.8))


















}



