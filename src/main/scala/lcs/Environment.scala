package lcs

class Condition {
  var data : Option[BitString] = None

  def set(bits: String): Unit = {
    data = Some(BitString(bits) )
  }
}

object Condition {
  def apply(bits:String): Condition = {
    val cnd : Condition = new Condition()
    cnd.set(bits)
    cnd
  }
}

class Phenotype {
  var data : Option[BitString] = None

  def set(bits: String): Unit = {
    data = Some(BitString(bits) )
  }
}

object Phenotype {
  def apply(bits:String): Phenotype = {
    val ptype : Phenotype = new Phenotype()
    ptype.set(bits)
    ptype
  }
}

class Environment {
  type Instance = (Condition,Phenotype)
  var filename = "/Users/yilmaz/IdeaProjects/SocialSim/src/main/scala/6MultiplexerData.txt"

  var  dm = new DataManagement()
  var data : Array[(Condition,Phenotype)] = dm.loadFile(filename,6).toArray
  var index : Int = 0
  def sense: Instance = {
    val instance = data(index)
    index = (index + 1) % data.length
    instance
  }

}
