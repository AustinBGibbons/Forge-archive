import maptest.compiler._
import maptest.library._
import maptest.shared._

// This object lets us run the Delite version of the code
object HelloWorldCompiler extends MapTestApplicationCompiler with HelloWorld 

// This object lets us run the Scala library version of the code
object HelloWorldInterpreter extends MapTestApplicationInterpreter with HelloWorld 

trait HelloWorld extends MapTestApplication { 
  def main() = {
    println("Hello, World!")
    val ms = MultiSet[Float]()
  
    ms.add(1.0)
    ms.add(1.0)
    ms.add(1.5)
    
    println("True? " + ms.contains(1.0))
    println("False? " + ms.contains(1.2))
    
    println("0? : " + ms.count(1.2))
    println("1? : " + ms.count(1.5))
    println("2? : " + ms.count(1.0))
  
    ms.remove(1.0)
    println("1? : " + ms.count(1.0))
    ms.remove(1.0)
    println("0? : " + ms.count(1.0))
  }
}
