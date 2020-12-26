import PythonUtilities.{executeModule, interp, loadModule}
import breeze.linalg._
import breeze.linalg.operators.BitVectorOps
import xcs._

//import lcs._


object SocialSimMain extends App {

  //loadModule("/Users/yilmaz/IdeaProjects/SocialSim/src/FCM.py",interp)

  //var result1 = executeModule("performExperiments")
  //println(result1)

/*
  var ucs : SupervisedLCS  = new SupervisedLCS()
  ucs.run(new MultiplexProblem(2000))
  ucs.report(cl => (cl.fitness > 0.8) && (cl.accuracy > 0.8))
*/


  var uslcs : UnSupervisedLCS with Reporter = new UnSupervisedLCS with Reporter {}
  uslcs.run(new MultiplexProblem(50000))
  var results = uslcs.report(cl => (cl.fitness > 0.5))
  println("--------Sorted Results ------ ")
  uslcs.sort(results) foreach {cl =>
    println(cl.report())
  }





























}



