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
/*
  var ucs : UCS = new UCS()
  //var mux : Scenario[Condition,Phenotype,Boolean] = new MultiplexProblem(2000)
  ucs.run(new MultiplexProblem(2000))
  ucs.report(cl => (cl.fitness > 0.8) && (cl.accuracy > 0.8))

*/

  var p1 = Phenotype("11").data.get.bits
  var p2 = Phenotype("00").data.get.bits
  var p3 = Phenotype("01").data.get.bits
  var p4 = Phenotype("10").data.get.bits

  var L : Set[String] = Set()
  L = Set(p1,p2,p3,p4)
  L foreach {a => println(a)}
  println("-------")
  var K : Set[String] = Set()
  K = Set(p1)
  K foreach {a => println(a)}
  println("-------")

  var actionSet = L.diff(K)
  actionSet foreach {a => println(a)}

  println("-------")
  var A = actionSet.toArray
  for (i <- A.indices) println(A(i))
















}



