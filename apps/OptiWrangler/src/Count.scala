import optiwrangler.compiler._
import optiwrangler.library._
import optiwrangler.shared._

object CountCompiler extends OptiWranglerApplicationCompiler with Count 
object CountInterpreter extends OptiWranglerApplicationInterpreter with Count 

trait Count extends OptiWranglerApplication { 
  def main() = {
    println("hello world")    
    //val col = Table(0, "").tableFromFile("/afs/cs.stanford.edu/u/gibbons4/data/small.csv")
    val col = Table(0, "").tableFromFile("/afs/cs.stanford.edu/u/gibbons4/data/medium_well.csv")
    // don't worry about these nulls
    val writeMe = col.cut("\"", null).cut("1", null).cut("3", null)//.split("5", null)
    println(writeMe(15,2))
  }
}
