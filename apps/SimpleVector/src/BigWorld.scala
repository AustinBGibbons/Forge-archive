import simplevector.compiler._
import simplevector.library._
import simplevector.shared._

object BigWorldCompiler extends SimpleVectorApplicationCompiler with BigWorld 
object BigWorldInterpreter extends SimpleVectorApplicationInterpreter with BigWorld 

trait BigWorld extends SimpleVectorApplication { 
  def main() = {
    println("hello world")    

    val v1 = Vector[Int](10)
    val v2 = Vector[Int](10)
 
    // defs
    var i = 0
    while (i < v1.length) {
      v1(i) = i*2
      v2(i) = i
      i += 1
    }
   
    // zip
    val v3 = v1+v2
    println("v3 = v1+v2")

    // map
    println("v5 = v3*5")
    val v5 = v3*5
    
    // reduce
    println("v5.sum:")
    val z = v5.sum
    println(z)
    
    // map,reduce,mapreduce
    val vc = v1.map(e => e+2)
    val vc2 = vc.mapreduce(e => e-1, (a,b) => a+b)
    println("vc2: " + vc2)
    
    // filter
    println("v6 = v1.filter(_ < 5)")
    val v6 = v1.filter(_ < 5)
  }
   
}
