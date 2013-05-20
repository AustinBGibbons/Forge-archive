import userset.compiler._
import userset.library._
import userset.shared._

object HelloSetCompiler extends UserSetApplicationCompiler with HelloSet 
object HelloSetInterpreter extends UserSetApplicationInterpreter with HelloSet 

trait HelloSet extends UserSetApplication { 
  def main() = {
    println("hello world")    
    //val v1 = Set[Int](10)
  }
}
