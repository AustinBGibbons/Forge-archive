import optiwrangler.compiler._
import optiwrangler.library._
import optiwrangler.shared._

object HelloWranglerCompiler extends OptiWranglerApplicationCompiler with HelloWrangler 
object HelloWranglerInterpreter extends OptiWranglerApplicationInterpreter with HelloWrangler 

trait HelloWrangler extends OptiWranglerApplication { 
  def main() = {
    println("hello world")    
  }
}
