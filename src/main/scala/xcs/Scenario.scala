package xcs


trait Scenario[Condition,Action,Reward] {
  var remainingCycles : Int
  var possibleActions : List[Action]
  var steps : Int
  def getPossibleActions : List[Action] = possibleActions
  def getActionData: Action => String
  def more : Boolean
  def reset : Unit
  def sense : (Condition,Action)
  def execute(action: String) : Reward
  def getRewardValue(r: Reward) : Double
  def eop : Boolean =  remainingCycles == 0
}

class MultiplexProblem(cycles: Int) extends Scenario[Condition,Phenotype,Double] {
  type Instance = (Condition,Phenotype)
  var filename = "/Users/yilmaz/IdeaProjects/SocialSim/src/main/scala/6MultiplexerData.txt"
  var  dm = new DataManagement()
  var data : Array[(Condition,Phenotype)] = dm.loadFile(filename,6).toArray
  var index : Int = 0
  var steps : Int = 0
  var currentSituation : Option[Instance] = None
  var remainingCycles : Int = cycles
  var possibleActions : List[Phenotype] = List(Phenotype("0"),Phenotype("1"))

  def more : Boolean = {
    remainingCycles > 0
  }

  def reset: Unit = {
    remainingCycles = cycles
  }

  def sense : (Condition,Phenotype) = {
    val instance = data(index)
    currentSituation = Some(instance)
    index = (index + 1) % data.length
    instance
  }


  def execute(action: String): Double = {
    var reward : Double = 0.0
    remainingCycles = remainingCycles - 1
    steps = steps + 1
    if (action.equals(currentSituation.get._2.data.get.bits)) reward = 1000.0
    else reward = 0.0
    reward
  }

  def getRewardValue(r: Double) : Double = r

  def getActionData : Phenotype => String = {p =>
    p.data.get.bits
  }

}

