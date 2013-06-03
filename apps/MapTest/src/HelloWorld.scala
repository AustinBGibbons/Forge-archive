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
    val ms = MultiSet()
  
    ms.ms_add(1.0f)
    ms.ms_add(1.0f)
    ms.ms_add(1.5f)
    
    println("True? " + ms.ms_contains(1.0f))
    println("False? " + ms.ms_contains(1.2f))
    
    println("0? : " + ms.ms_count(1.2f))
    println("1? : " + ms.ms_count(1.5f))
    println("2? : " + ms.ms_count(1.0f))
  
    ms.ms_remove(1.0f)
    println("1? : " + ms.ms_count(1.0f))
    ms.ms_remove(1.0f)
    println("0? : " + ms.ms_count(1.0f))
  }
}
