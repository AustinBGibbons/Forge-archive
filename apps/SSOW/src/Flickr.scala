import ssow.compiler._
import ssow.library._
import ssow.shared._

object FlickrCompiler extends SSOWApplicationCompiler with Flickr 
object FlickrInterpreter extends SSOWApplicationInterpreter with Flickr 

trait Flickr extends SSOWApplication { 
  def main() = {
    println("hello world")    
    println(args(0))
    val v = Vector(args(0)) 
    val now = clock()
    v.map(row => row.map(cell => cell.replaceAllLiterally("\"", "")))
      .map(row => row.drop(0)).map(row => row.drop(4))
    println("size: " + v.length)
    println("clock-a-doodle-doo: " + ((clock() - now) / 1e3))
  }
}
