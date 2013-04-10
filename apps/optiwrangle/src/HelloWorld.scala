import optiwrangle.compiler._
import optiwrangle.library._
import optiwrangle.shared._

object HelloSimpleCompiler extends OptiWrangleApplicationCompiler with HelloSimple 
object HelloSimpleInterpreter extends OptiWrangleApplicationInterpreter with HelloSimple 

trait HelloSimple extends OptiWrangleApplication { 
  def main() = {
    println("hello world")    

    val inFile = "/afs/cs.stanford.edu/u/gibbons4/data/flickr.in.csv"
    var dw = DataWrangler(inFile, ",")
    dw.promote(1)
    val outFile = "/afs/cs.stanford.edu/u/gibbons4/data/flickr.out.csv"
    dw.write(outFile, ",")
  } 
}
