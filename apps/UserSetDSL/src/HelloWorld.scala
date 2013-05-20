import usersetdsl.compiler._
import usersetdsl.library._
import usersetdsl.shared._

object HelloSetCompiler extends UserSetDSLApplicationCompiler with HelloSet 
object HelloSetInterpreter extends UserSetDSLApplicationInterpreter with HelloSet 

trait HelloSet extends UserSetDSLApplication { 
  def main() = {
    println("hello world")    
    val vset = XSet[Int]()
    vset.put(3)
    vset.put(3)
    vset.put(4)
    val v1 = UserSet[Int](vset)
    println(v1.userContains(3))
    println(v1.userContains(5))
  }
}
