import PythonUtilities.{executeModule, interp, loadModule}

import breeze.linalg._
import lcs._



object SocialSimMain extends App {
  print("hello")

  loadModule("/Users/yilmaz/IdeaProjects/SocialSim/src/FCM.py",interp)

  //var result1 = executeModule("performExperiments")
  //println(result1)

  var bs1 : BitString = new BitString("001100110011")
  println(bs1.data())

  var bs2 : BitString = new BitString("001100110011")
  println(bs2.data())


  var bv1 = bs1.data()

  println("length: " + bv1.length)

  var bv2 = bs2.data()

  if (bv1.equals(bv2)) println("equal")

  bv2.update(0,true)

  println("updated....")
  if (!bv1.equals(bv2)) println("not equal")

  bv1.iterator foreach {x =>
    println(x._1 + " -> " + x._2)
  }




}



