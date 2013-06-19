import cvwrangler.compiler._
import cvwrangler.library._
import cvwrangler.shared._

object GeneCompiler extends CVWranglerApplicationCompiler with Gene 
object GeneInterpreter extends CVWranglerApplicationInterpreter with Gene 

trait Gene extends CVWranglerApplication { 
  def main() = {
    println("hello world")    
    val t1 = Table(args(0))
    val now = clock(t1)
    val t2 =  t1
 //     .promote
 //     .partition(_.substring(4,8), 1)
      .delete(clip, 1)
      .cutBefore(13, 1)
      .force
    println("Total time : " + ((clock(t2)-now) / 1e3))
  }
}
