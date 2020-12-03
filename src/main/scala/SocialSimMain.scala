import PythonUtilities.{executeModule, interp, loadModule}

import breeze.linalg._
import breeze.linalg.operators.BitVectorOps
import lcs._
import BitString._



object SocialSimMain extends App {

  //loadModule("/Users/yilmaz/IdeaProjects/SocialSim/src/FCM.py",interp)

  //var result1 = executeModule("performExperiments")
  //println(result1)

  var bs1 : BitString = BitString("001100110011")
  var bs2 : BitString = BitString("0011##110011")

  println(bs2.encode())
  println(decode(bs2))










}



