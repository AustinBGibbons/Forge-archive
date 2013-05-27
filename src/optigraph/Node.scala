package ppl.dsl.forge
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}
import examples._

//object NodeOpsRunner extends ForgeApplicationRunner with NodeOps

trait NodeOps extends ForgeApplication with ScalaOps {
  /**
   * The name of your DSL. This is the name that will be used in generated files,
   * package declarations, etc.
   */
  //def dslName = "NodeOps"
  
  /**
   * The specification is the DSL definition (types, data structures, ops)
   */
  /*
  def specification() = {
    addScalaOps() 
    addNodeOps()
  }
  */
  
  def addNodeOps() {            
    addScalaOps()
  
    // generic type parameters we will use 
    val T = tpePar("T") 
    val Node = tpe("Node", T) 

    // data fields     
    data(Node, ("_data", T))      
  
    // allocation
    static (Node) ("apply", T, T :: Node(T), effect = mutable) implements allocates (Node, ${$0})
    
    // doesn't rewrite correctly if we use "withTpe (Node) {", but works if we use:
    val NodeOpsImpl = withTpe (Node)
     
    //infix (Node) ("toString", Nil, Nil :: MString) implements codegen ($cala, ${})
    //infix (Node) ("toString", Nil, Nil :: MString) implements codegen ($cala, ${node_data($self).toString})
     
    NodeOpsImpl {
      compiler ("node_data") (Nil :: T) implements getter (0, "_data")
      compiler ("node_set_data") (T :: MUnit, effect=write(0)) implements setter (0, "_data", quotedArg(1))
    }                    

    ()    
  }
}
 
