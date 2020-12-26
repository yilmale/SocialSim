package xcs

import Constants._
import lcs.BitString

class ClassifierSet extends ClassifierPopulation[Condition,Phenotype,Double] {
  def getActionData : Phenotype => String = {p =>
    p.data.get.bits
  }

  def generateAction : String => Phenotype = {s =>
    Phenotype(s)
  }

  def initializeClassifier : (Condition,Int) => ClassifierType = {(c,t) =>
    val cnd : Condition = new Condition()
    val context : String = c.data.get.bits
    var str = new StringBuilder("")

    for (i <- 0 until context.length) {
      if (rng.nextDouble() <= p_wc)
        str = str += '#'
      else
        str = str += context(i)
    }
    cnd.set(str.toString())
    new Classifier(cnd, t)
  }

  def classifierGenerator(cl: ClassifierType) : ClassifierType = {
    val gen = new Classifier(Condition(cl.condition.data.get.bits),
      cl.timeStampGA
    )
    gen.action = Phenotype(cl.action.data.get.bits)
    gen.timeStampGA = cl.timeStampGA
    gen.prediction = cl.prediction
    gen.error = cl.error
    gen.fitness = cl.fitness
    gen.experience = 0
    gen.timeStamp = cl.timeStamp
    gen.actionSetSize = cl.actionSetSize
    gen.numerosity = 1
    gen
  }

  def applyMutation(cl: ClassifierType,c: Condition): Unit = {
    var sb1 : StringBuilder = new StringBuilder("")
    val cnd : String = cl.condition.data.get.bits
    val env : String = c.data.get.bits
    val size : Int = cl.condition.data.get.bits.length
    var i : Int = 0
    do {
      if (rng.nextDouble() < upsilon) {
        if (cnd(i) == '#')
          sb1 = sb1 += env(i)
        else
          sb1 = sb1 += '#'
      }
      else sb1 = sb1 += cnd(i)
      i = i + 1
    } while (i < size)
    cl.condition = Condition(sb1.toString())
    if (rng.nextDouble() < 0.5) {
      cl.action = Phenotype(BitString.randomBitString(cl.action.data.get.bits.length).bits)
    }
  }

  def applyCrossOver(cl1: ClassifierType, cl2: ClassifierType): Unit  = {
    val size : Int = cl1.condition.data.get.bits.length
    var x :Int = (rng.nextDouble() * size).toInt
    var y : Int = (rng.nextDouble() * size).toInt

    if(x >y) {
      val temp = y
      y = x
      x = temp
    }

    var i = 0
    val str1 : String = cl1.condition.data.get.bits
    val str2 : String = cl2.condition.data.get.bits

    var sb1 : StringBuilder = new StringBuilder("")
    var sb2 : StringBuilder = new StringBuilder("")
    do {
      if ((x <= i) && (i < y)) {
        sb1 = sb1 += str2(i)
        sb2 = sb2 += str1(i)
      }
      else {
        sb1 = sb1 += str1(i)
        sb2 = sb2 += str2(i)
      }
      i = i+1
    } while (i < size)

    cl1.condition = Condition(sb1.toString())
    cl2.condition = Condition(sb2.toString())
  }


}
