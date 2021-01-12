import jep.SharedInterpreter
import jep.python.PyCallable

object PythonUtilities {

  type InputVector = java.util.ArrayList[Any]
  val interp = initPythonInterpreter

  def getParams(inp: List[Any]): InputVector = {
    var inputParams : InputVector = new java.util.ArrayList()
    inp foreach {i => inputParams.add(i)}
    inputParams
  }

  def getParams(inp: Map[String,Any]): InputVector = {
    var inputParams : InputVector = new java.util.ArrayList()
    inp.values foreach {i => inputParams.add(i)}
    inputParams
  }

  def executeModule(name: String, p: List[Any]) : Any = {
    var mdl = interp.getValue(name,classOf[PyCallable])
    if (p.isEmpty) mdl.call()
    else mdl.call(getParams(p))
  }

  def executeModule(name: String, p: Map[String,Any]) : Any = {
    var mdl = interp.getValue(name,classOf[PyCallable])
    if (p.isEmpty) mdl.call()
    else mdl.call(getParams(p))
  }

  def executeModule(name: String) : Any = {
    var mdl = interp.getValue(name,classOf[PyCallable])
    mdl.call()
  }

  def initPythonInterpreter: SharedInterpreter = {
    new SharedInterpreter()
  }

  def loadModule(pathName: String, interp: SharedInterpreter): Unit = {
    interp.runScript(pathName)
  }
}