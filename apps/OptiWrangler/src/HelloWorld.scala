import optiwrangler.compiler._
import optiwrangler.library._
import optiwrangler.shared._

object HelloWranglerCompiler extends OptiWranglerApplicationCompiler with HelloWrangler 
object HelloWranglerInterpreter extends OptiWranglerApplicationInterpreter with HelloWrangler 

trait HelloWrangler extends OptiWranglerApplication { 
  def main() = {
    println("hello world")    
    //println(args(0))
    //val col = Table(0, "").tableFromFile("/afs/cs.stanford.edu/u/gibbons4/data/singleCol.txt")
    val now = System.nanoTime
    val col = Table(0, "").tableFromFile(args(0))
    println("Loaded from file : " + ((System.nanoTime - now) / 1e6))
   // col(0) = Array("1")
    //col(2) = Array("3")
  
    val now2 = System.nanoTime
    val x = col.cutAll("\"").cut("1")
    println("Cut : " + ((System.nanoTime - now2) / 1e6))
    
    val now3 = System.nanoTime
    x.tableToFile("/home/gibbons4/data/")
    println("Written to file : " + ((System.nanoTime - now3) / 1e6))

    println("just kidding none of that is blocking.")
/*
    println(cutcol(0,0))
    println(cutcol(1, 0))

    println(cutcol.getColumn(1))
    println(cutcol.getColumn("2"))
*/
/*
    val splitcol = col.split(1, null)
    println(cutcol(0, 0) + "," + cutcol(0, 1))
    println(cutcol(1, 0) + "," + cutcol(1, 1))

    cutcol.putHeader("one", 1)
    cutcol.putHeader("two", 1)
    
    println(cutcol.getHeader("one"))
*/
  }
}
