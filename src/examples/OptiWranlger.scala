package ppl.dsl.forge
package examples

import core.{ForgeApplication,ForgeApplicationRunner}

object OptiWranglerDSLRunner extends ForgeApplicationRunner with OptiWranglerDSL

trait Base extends ForgeApplication {
  def addErrorChecking() {
    val Error = grp("Error")

    direct (Error) ("goodbye", Nil, MString :: MUnit, effect = simple) implements codegen ($cala, ${
      //System.err.println("\n\n\t" + $0 + "\n\n")
      System.err.println("\t" + $0 + "")
      System.exit(-1)
    })
  }
}

trait OptiWranglerDSL extends Base {
  def dslName = "OptiWrangler"
  
  def specification() = {
    importScalaOps()
    addErrorChecking()
    //addWranglerOps()
    addTableOps()
  }

  def addTableOps() {
    val Table = tpe("Table")
    val SArray = tpeInst(MArray, MString)
    val SSArray = tpeInst(MArray, SArray) 
    val MSI = tpeInst(MMap, List(MString, MInt))
  
    data(Table, ("data", SSArray), ("width", MInt), ("header", MSI), ("name", MString))

    // allocators - I think I'm going to move codegen to underneath spec
    
    val TableOps = withTpe (Table)
    TableOps {
      // ops
    }
  }
}
