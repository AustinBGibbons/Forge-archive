import optigraph.compiler._
import optigraph.library._
import optigraph.shared._

object HelloSetCompiler extends OptiGraphApplicationCompiler with HelloSet 
object HelloSetInterpreter extends OptiGraphApplicationInterpreter with HelloSet 

trait HelloSet extends OptiGraphApplication { 
  def main() = {
    println("hello world")    
    val v1 = Graph[Int]()
    v1.addEdge(1, 3)
    v1.addEdge(1, 2)
    v1.addEdge(2, 3)
  }
}
