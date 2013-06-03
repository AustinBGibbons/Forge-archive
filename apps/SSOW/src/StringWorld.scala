import ssow.compiler._
import ssow.library._
import ssow.shared._

object HelloStringCompiler extends SSOWApplicationCompiler with HelloString 
object HelloStringInterpreter extends SSOWApplicationInterpreter with HelloString 

trait HelloString extends SSOWApplication { 
  def main() = {
    //println("hello world")    

    val alloc = clock()
  
    val v2 = Vector(args(0))

    //v2.map(_.pprint)
    println("Alloc time: " + ((clock() - alloc) / 1e3))
///*
    val now = clock()
    val v1 = v2.map(x => x.map(_.replaceAllLiterally("\"", "")))
                .map(x => x.map(_.replaceFirst("1", "")))
                .filter(x => {x.length != 0 && x(0) != "2"})
    println("length: " + v1.length)
    v1(0).pprint
    println("The time is now: " + ((clock() - now) / 1e3))
//*/
  }
}
