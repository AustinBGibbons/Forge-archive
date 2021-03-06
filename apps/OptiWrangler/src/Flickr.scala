import optiwrangler.compiler._
import optiwrangler.library._
import optiwrangler.shared._

object FlickrCompiler extends OptiWranglerApplicationCompiler with Flickr 
object FlickrInterpreter extends OptiWranglerApplicationInterpreter with Flickr 

trait Flickr extends OptiWranglerApplication { 
  def main() = {
    println("hello world")    
    println(args(0))
    val col = Table(0, "").tableFromFile(args(0))
    def rm(x: String)( y: Rep[String]) = /*col.*/regexMatch(x, y)
    //col.promote(0).cutAll("\"").drop(0).drop(4).delete(rm("1[0-8]") _, 0)
    val now = clock()
    val f = col.cutAll("\"").drop(0).drop(4)//.delete(rm("1[0-8]") _, 0)
    println("clock-a-doodle-doo: " + ((clock() - now) / 1e3.toDouble))
    f.tableToFile("/home/gibbons4/data/")
  }
}
