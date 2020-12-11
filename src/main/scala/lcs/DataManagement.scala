package lcs

import scala.io.Source


class DataManagement {
  def loadFile(filename: String,index: Int): List[(Condition, Phenotype)] = {
    //var filename = "/Users/yilmaz/IdeaProjects/SocialSim/src/main/scala/6MultiplexerData.txt"
    var data : List[(Condition, Phenotype)] = List()
    val bufferedSource = Source.fromFile(filename)
    var dataFrame : List[String] = List()
    for (line <- bufferedSource.getLines()) {
      var sb : StringBuilder = new StringBuilder("")
      line.split("\\s+") foreach {str =>
        sb = sb ++= str
      }
      dataFrame = sb.toString() ::  dataFrame
    }

    dataFrame.reverse.tail foreach {bs =>
        var cnd = Condition(bs.substring(0,index))
        var pt = Phenotype(bs.substring(index))
        data = (cnd,pt) :: data
    }

    bufferedSource.close()
    data.reverse
  }

}
