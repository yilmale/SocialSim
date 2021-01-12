import PythonUtilities.{executeModule, interp, loadModule}
import breeze.linalg._
import breeze.linalg.operators.BitVectorOps
//import xcs._

//import lcs._

import scalaSim._
import scalAgent._
import  iac._
import iac.Declarations._


object SocialSimMain extends App {

  DESimTest()
  println("--------")

  new Experiment {
    params = Map("stopTime" -> 30)
    new Context {
      model = new Model {
        add(new Agent{agentType = "Normative "
          var x : Int = 0
          object NormRG extends ActivityGenerator

          val increment = NormRG.Gen(true) {
            println("Normative Agent " + agentType + "--- increment")
          } (false)

          val decrement = NormRG.Gen(true) {
            println("Normative Agent " + agentType + "--- decrement")
          } (false)

          addActivity(increment)
          addActivity(decrement)})
      }
    }
  }.simulate()

  println("------------------------")

  var n = Narrative {
    evidence("e", 0.2)
    evidence("e1", 1.0)
    belief("a", 0.5)
    belief("b", 0.2)
    belief("c", 0.4)
  } subjectTo {
    explain("b", "e", 0.5)
    explain(List("a", "c"), "e1", 0.5)
    contradict("a", "b", 1)
  }

  println(n.activations)
  println(n.constraintMatrix)

  n.simulate()
  println(n.activations)

  println("---------------------")

  IACDSLTest()

  IACUtilities()

  //loadModule("/Users/yilmaz/IdeaProjects/SocialSim/src/FCM.py",interp)

  //var result1 = executeModule("performExperiments")
  //println(result1)

/*
  var ucs : SupervisedLCS  = new SupervisedLCS()
  ucs.run(new MultiplexProblem(2000))
  ucs.report(cl => (cl.fitness > 0.8) && (cl.accuracy > 0.8))
*/

/*
  var uslcs : UnSupervisedLCS with Reporter = new UnSupervisedLCS with Reporter {}
  uslcs.run(new MultiplexProblem(50000))
  var results = uslcs.report(cl => (cl.fitness > 0.5))
  println("--------Sorted Results ------ ")
  uslcs.sort(results) foreach {cl =>
    println(cl.report())
  }

*/

}



