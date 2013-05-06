import stringwrangler.compiler._
import stringwrangler.library._
import stringwrangler.shared._

object HelloWranglerCompiler extends StringWranglerApplicationCompiler with HelloWrangler 
object HelloWranglerInterpreter extends StringWranglerApplicationInterpreter with HelloWrangler 

trait HelloWrangler extends StringWranglerApplication { 
  def main() = {
    println("hello world")    
    
    val col = Column(3)

    col(0) = "0"
    col(2) = "2"

    println(col(0))
    println(col(1))
  }
}
