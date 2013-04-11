import optivega.compiler._
import optivega.library._
import optivega.shared._

object HelloWorldCompiler extends OptiVegaApplicationCompiler with HelloWorld 
object HelloWorldInterpreter extends OptiVegaApplicationInterpreter with HelloWorld 

trait HelloWorld extends OptiVegaApplication { 
  def main() = {
    println("hello world")

    val vega = Vega()
    println(vega.plot())
  }
}
