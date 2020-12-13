package lcs

import Constants._

class ClassifierSet extends ClassifierPopulation[Condition,Phenotype,Boolean] {
  def getActionData : Phenotype => String = {p =>
    p.data.get.bits
  }

  def classifierCover: (Int, Condition, Phenotype, Int) => ClassifierType = {(setSize,situation,act,time) =>
      val cnd : Condition = new Condition()
      val context : String = situation.data.get.bits
      var str = new StringBuilder("")

      for (i <- 0 until context.length) {
        if (rng.nextDouble() <= p_wc)
          str = str += '#'
        else
          str = str += context(i)
      }
      cnd.set(str.toString())
      new Classifier(setSize,cnd, act, time)
    }

  def classifierGenerator: ClassifierType => ClassifierType = {cl=>
    val gen = new Classifier(1,
      Condition(cl.condition.data.get.bits),
      Phenotype(cl.action.data.get.bits),
      cl.timeStampGA
    )
    gen.timeStampGA = cl.timeStampGA
    gen.matchCount = cl.matchCount
    gen.correctCount = cl.correctCount
    gen.avgMatchSize = cl.avgMatchSize
    gen
  }

  def applyMutation(cl: ClassifierType,c: Condition, p: Phenotype): Unit = {
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

  def applyCrossOver(cl1: ClassifierType, cl2: ClassifierType): Unit = {
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

