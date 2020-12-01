package lcs

import breeze.linalg._

trait BitStringBase {
  var bv : BitVector
  def random(length: Int, bitProb: Double): Unit = {
    
  }
  def mutate(): Unit =  {

  }

  def translate(bv: BitVector) : String = {
    var str : String = ""
    bv.iterator foreach {b =>
      if (b._2) str = str + "1"
      else str = str + "0"
    }
    str
  }

}

class BitString(bits: String) extends BitStringBase {
  var bv : BitVector = BitVector(bits.length)()
  0 until bits.length foreach {i =>
    if (bits(i) == '1') bv.update(i,true)
  }

  def data(): BitVector = {
    bv
  }

}

object BitString {
  def apply(bits: String): BitString = {
    new BitString(bits)
  }
}