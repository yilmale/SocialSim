package xcs

trait Scenario[Condition,Action,Reward] {
  var remainingCycles : Int
  var possibleActions : List[Action]
  var steps : Int
  def getPossibleActions : List[Action] = possibleActions
  def more : Boolean
  def reset : Unit
  def sense : Condition
  def execute(action: Action) : Reward
  def eop : Boolean =  remainingCycles == 0
}