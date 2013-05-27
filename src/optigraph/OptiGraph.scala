package ppl.dsl.forge
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}
import examples._

object OptiGraphRunner extends ForgeApplicationRunner with OptiGraph

trait OptiGraph /*extends ForgeApplication with ScalaOps*/ extends NodeOps {
  /**
   * The name of your DSL. This is the name that will be used in generated files,
   * package declarations, etc.
   */
  def dslName = "OptiGraph"
  
  /**
   * The specification is the DSL definition (types, data structures, ops)
   */
  def specification() = {
    //addScalaOps() 
    addNodeOps() 
    addGraphOps()
  }
  
  def addGraphOps() {            
    // generic type parameters we will use 
    val T = tpePar("T") 
    val Graph = tpe("Graph", T) 
    val Node = lookupTpe("Node")
    val GMap = tpeInst(MMap, List(Node(T),Node(T)))

    // data fields     
    data(Graph, ("_data", GMap))      
  
    // allocation
    static (Graph) ("apply", T, Nil :: Graph(T), effect = mutable) implements allocates (Graph, ${ map_empty[Node[T], Node[T]]()})
    
    // doesn't rewrite correctly if we use "withTpe (Graph) {", but works if we use:
    val GraphOps = withTpe (Graph)
          
    GraphOps {
      compiler ("graph_data") (Nil :: GMap) implements getter (0, "_data")
      compiler ("graph_set_data") (GMap :: MUnit, effect=write(0)) implements setter (0, "_data", quotedArg(1))
    }

    ()    
  }
}
 
