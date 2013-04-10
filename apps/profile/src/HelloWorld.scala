import profile.compiler._
import profile.library._
import profile.shared._

object HelloProfileCompiler extends ProfileApplicationCompiler with HelloProfile 
object HelloProfileInterpreter extends ProfileApplicationInterpreter with HelloProfile 

trait HelloProfile extends ProfileApplication { 
  def main() = {
    //println("hello world")    

    var acc = 0.
    val time = 
      Profile (100) times {
        for (i <- 0 until 100000) {
          acc = acc + Math.exp(i)*Math.pow(i,10.0)*42.0
        }
      } //report average
    //println("first loop time: " + time(0))
    println(time.plot())
  }   
}
