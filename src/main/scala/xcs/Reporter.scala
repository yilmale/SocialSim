package xcs

trait Reporter {

  def reportCondition(c: Condition): String = {
    var str : String = c.data.get.bits
    s"Condition:  $str"
  }

  def reportAction(p: Phenotype): String = {
    var str : String = p.data.get.bits
    s"Action:  $str"
  }

  def reportSet(s: List[ClassifierRule[Condition,Phenotype,Double]]): Unit = {
    s. foreach {c =>
      println(c.report())
    }
  }

  def reportPredictionArray(pa: scala.collection.mutable.Map[String,Option[Double]]): Unit = {
    pa foreach {p =>
      println(p)
    }
  }


  def sort : List[ClassifierRule[Condition,Phenotype,Double]] =>
    List[ClassifierRule[Condition,Phenotype,Double]] = {cls =>
    isort(cls).reverse
  }


  def isort : List[ClassifierRule[Condition,Phenotype,Double]] =>
    List[ClassifierRule[Condition,Phenotype,Double]] = {cls =>
    def insert(x: ClassifierRule[Condition,Phenotype,Double],
               xs: List[ClassifierRule[Condition,Phenotype,Double]]):
            List[ClassifierRule[Condition,Phenotype,Double]] = {
      xs match {
        case List() => List(x)
        case y :: ys => if (x.prediction <= y.prediction) x :: xs else y :: insert(x,ys)
      }
    }

    cls match {
      case List() => List()
      case x :: xs => insert(x,isort(xs))
    }
  }


}
