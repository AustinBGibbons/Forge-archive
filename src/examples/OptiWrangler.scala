package ppl.dsl.forge
package examples

import core.{ForgeApplication,ForgeApplicationRunner}

object OptiWranglerDSLRunner extends ForgeApplicationRunner with OptiWranglerDSL

trait OptiWranglerDSL extends ForgeApplication with ScalaOps {
  /**
   * The name of your DSL. This is the name that will be used in generated files,
   * package declarations, etc.
   */
  def dslName = "OptiWrangler"
  
  /**
   * The specification is the DSL definition (types, data structures, ops)
   */
  def specification() = {
    /**
     * Include Scala ops
     */
    addScalaOps() 
    
    /**
     * The main portion of our DSL
     */        
    addWranglerOps()
  }
  
  
  def addWranglerOps() {            
    // generic type parameters we will use 
     val T = tpePar("T") 
    // val R = tpePar("R")
    
    val Wrangler = tpe("Wrangler", T) 
    //val SArray = tpeInst(MArray, MString)

    val SArray = tpeInst(MArray, MString)
    val SSArray = tpeInst(MArray, SArray)

/*
    val SArrayOps = withTpe (SArray)
    SArrayOps {
      data(("_data", MInt))
    }
*/

    val WranglerOps = withTpe (Wrangler)
    WranglerOps {                  
      // data fields     
      data(("_length", MInt), ("_data", SSArray))

      // allocation
      op (Wrangler) ("apply", static, T, (MInt), Wrangler, effect = mutable) implements allocates(Wrangler, ${$0}, /*${ array_empty[SArray]($0)*/  ${null} )
      
      // getters and setters
      "data" is (compiler, Nil, SSArray) implements getter(0, "_data")
      "length" is (infix, Nil, MInt) implements getter(0, "_length")
      
      // data ops             
      "apply" is (infix, (MInt), SArray) implements composite ${ array_apply(data($self), $1) }                        
      // example named arg
      "update" is (infix, (("i",MInt),("e",SArray)), MUnit, effect = write(0)) implements composite ${
        array_update(data($self), $i, $e)
      }

      parallelize as ParallelCollection(SArray,
       lookupOverloaded("apply",1),
       lookup("length"),
       lookupOverloaded("apply",0),
       lookup("update") 
      )            

      "potato" is (compiler, (tpe("ForgeArray", T)), MUnit) implements single ${}
    }                    
                                        
    ()    
  }
}
 
