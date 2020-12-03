package lcs

import breeze.linalg._

object Ternary extends Enumeration {
  type Ternary = Value
  val True, False, WC, NULL = Value
}
import Ternary._

class BitString(val bits: String) {
  type WCSet = java.util.BitSet
  var bv : BitVector = BitVector(bits.length)()
  var wc : WCSet = new java.util.BitSet()

  0 until bits.length foreach {i =>
    if (bits(i) == '1') bv.update(i,v = true)
    else if (bits(i) == '#') wc.set(i)
  }

  def encode(): (BitVector,WCSet) = {
    (bv,wc)
  }

  def length: Int = {
    bv.length
  }

  def getItem(index: Int): Ternary = {
    var itemType : Ternary = NULL
    if (bv.activeKeysIterator contains(index)) itemType = True
    else
    if (wc.get(index)) itemType = WC
    else itemType = False
    itemType
  }

}

object BitString {
  var rng : scala.util.Random = new scala.util.Random()
  def apply(bits: String): BitString = {
    new BitString(bits)
  }

  def randomBitString(length: Int, bitProb: Double = 0.5): BitString = {
    var str = new StringBuilder("")
    for (i <- 0 until length) {
      if (rng.nextDouble() <= bitProb)
        str = str += '1'
      else
        str = str += '0'
    }
    var bs : BitString = new BitString(str.toString())
    bs
  }

  def decode(bs: BitString) : String = {
    var str = new StringBuilder("")
    0 until bs.bits.length foreach {i =>
      bs.getItem(i) match {
        case True => str = str += '1'
        case False => str = str += '0'
        case WC => str = str += '#'
        case _ =>
      }
    }
    str.toString()
  }

}