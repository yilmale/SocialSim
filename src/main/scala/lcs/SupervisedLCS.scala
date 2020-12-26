package lcs
import Constants._

class SupervisedLCS extends sLCS[Condition,Phenotype,Boolean] {
  val classifierEnsemble = new ClassifierSet()
}

