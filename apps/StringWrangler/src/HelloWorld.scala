import stringwrangler.compiler._
import stringwrangler.library._
import stringwrangler.shared._

object HelloWranglerCompiler extends StringWranglerApplicationCompiler with HelloWrangler 
object HelloWranglerInterpreter extends StringWranglerApplicationInterpreter with HelloWrangler 

trait HelloWrangler extends StringWranglerApplication { 
  def main() = {
    println("hello world")    
    
    //val col = Column("/afs/cs.stanford.edu/u/gibbons4/data/singleCol.txt")
    val col = Column(0).columnFromFile("/afs/cs.stanford.edu/u/gibbons4/data/singleCol.txt")

    col(0) = "1"
    col(2) = "3"

    val cutcol = col.cut(1)

    println(cutcol(0))
    println(cutcol(1))
  }
}
