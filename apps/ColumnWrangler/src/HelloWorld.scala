import columnwrangler.compiler._
import columnwrangler.library._
import columnwrangler.shared._

object HelloColumnCompiler extends ColumnWranglerApplicationCompiler with HelloColumn 
object HelloColumnInterpreter extends ColumnWranglerApplicationInterpreter with HelloColumn 

trait HelloColumn extends ColumnWranglerApplication { 
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
