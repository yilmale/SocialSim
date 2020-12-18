import PythonUtilities.{executeModule, interp, loadModule}
import breeze.linalg._
import breeze.linalg.operators.BitVectorOps
import lcs._
import BitString._
//import SocialSimMain.MyAction.{A1, A2, MyAction}
import lcs.Constants.beta
import xcs.Constants.rng

import scala.io.Source




object SocialSimMain extends App {

  //loadModule("/Users/yilmaz/IdeaProjects/SocialSim/src/FCM.py",interp)

  //var result1 = executeModule("performExperiments")
  //println(result1)

  var ucs : UCS = new UCS()
  //var mux : Scenario[Condition,Phenotype,Boolean] = new MultiplexProblem(2000)
  ucs.run(new MultiplexProblem(2000))
  ucs.report(cl => (cl.fitness > 0.8) && (cl.accuracy > 0.8))


/*
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
  println("-------")
  var idx : Int = xcs.Constants.rng.nextInt(A.length)
  println(A(idx))

  object MyAction extends Enumeration {
    type MyAction = Value

    val A1 : Value = Value("A1")
    val A2 : Value = Value("A2")
    val A3 : Value = Value("A3")
  }


  var X : scala.collection.mutable.Map[MyAction,Double] = scala.collection.mutable.Map[MyAction,Double]()
  X = X + (A1 ->5.0)
  X = X + (A2 -> 12.0)
  X(A1)= 10.0

  var a = X.keys.toArray
  println(a(rng.nextInt(X.keys.size)))
  var b = a(rng.nextInt(X.keys.size))

  if (b.equals(A1)) println(b + " is equal to " + A1)
  else {
    println(b + " is not equal to " + A1)
  }

*/
























}



