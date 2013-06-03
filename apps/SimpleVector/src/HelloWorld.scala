import simplevector.compiler._
import simplevector.library._
import simplevector.shared._

object HelloSimpleCompiler extends SimpleVectorApplicationCompiler with HelloSimple 
object HelloSimpleInterpreter extends SimpleVectorApplicationInterpreter with HelloSimple 

trait HelloSimple extends SimpleVectorApplication { 
  def main() = {
    //println("hello world")    

    val alloc = clock()
    val size = 1e7.toInt
  
    val v1 = Vector[Int](size)
    val v2 = Vector[Int](size)
 
    val a = Vector[Int](args(0))
   
    // defs
    var i = 0    
    while (i < v1.length) {
      v1(i) = a(i)*2
      v2(i) = a(i)
      i += 1
    }
    println("Alloc time: " + ((clock() - alloc) / 1e3))
    
    val now = clock()

    // zip
    val v3 = v1+v2
    // map
    val v5 = v3*5
    // reduce
    val z = v5.sum
    println(z)
    
    // map,reduce,mapreduce
    val vc = v1.map(e => e+2)
    val vc2 = vc.mapreduce(e => e-1, (a,b) => a+b)
    println("vc2: " + vc2)
    
    // filter
    val v6 = v1.filter(_ < 5)
    println("filtered sum : " + v6.sum)   
    println("The time is now: " + ((clock() - now) / 1e3))
  }
}
