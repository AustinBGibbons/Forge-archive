import optiwrangler.compiler._
import optiwrangler.library._
import optiwrangler.shared._

object HelloWranglerCompiler extends OptiWranglerApplicationCompiler with HelloWrangler 
object HelloWranglerInterpreter extends OptiWranglerApplicationInterpreter with HelloWrangler 

trait HelloWrangler extends OptiWranglerApplication { 
  def main() = {
    println("hello world")    
    val col = Table(0, "").tableFromFile("/afs/cs.stanford.edu/u/gibbons4/data/singleCol.txt")
    //val col = Table("/afs/cs.stanford.edu/u/gibbons4/data/singleCol.txt")

   // col(0) = Array("1")
    //col(2) = Array("3")

    val cutcol = col.cut(1)

    println(cutcol(0,0))
    println(cutcol(1, 0))
  }
}
