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
    
  def addClock() {
    val Clock = grp("Clock")
  
    direct (Clock) ("clock", Nil, MUnit :: MInt, effect = simple) implements codegen ($cala, ${
      System.currentTimeMillis().toInt
    })
  }
}

trait OptiWranglerDSL extends Base {
  def dslName = "OptiWrangler"
  
  def specification() = {
    importScalaOps()
    addErrorChecking()
    addClock()
    //addWranglerOps()
    addTableOps()
  }

  def addTableOps() {
    val Table = tpe("Table")
    val SArray = tpeInst(MArray, MString)
    val SSArray = tpeInst(MArray, SArray) 
    val MSI = tpeInst(MMap, List(MString, MInt))
  
    data(Table, ("_data", SSArray), ("_length", MInt), ("_width", MInt), ("_header", MSI), ("_name", MString))

    // allocators - I think I'm going to move codegen to underneath spec
    
    val TableOps = withTpe (Table)
    TableOps {
      // ops
      compiler ("data") (Nil :: SSArray) implements getter(0, "_data")
      compiler ("header") (Nil :: MSI) implements getter(0, "_header")
      compiler ("length") (Nil :: MInt) implements getter(0, "_length")
      compiler ("width") (Nil :: MInt) implements getter(0, "_width")
      compiler ("name") (Nil :: MString) implements getter(0, "_name")
      compiler ("set_data") (SSArray :: MUnit, effect=write(0)) implements setter(0, "_data", quotedArg(1))
      compiler ("set_header") (MSI :: MUnit, effect=write(0)) implements setter(0, "_header", quotedArg(1))
      compiler ("set_length") (MInt :: MUnit, effect=write(0)) implements setter(0, "_length", quotedArg(1))
      compiler ("set_width") (MInt :: MUnit, effect=write(0)) implements setter(0, "_width", quotedArg(1))
      compiler ("set_name") (MString :: MUnit, effect=write(0)) implements setter(0, "_name", quotedArg(1))

      
    }

    direct (Table) ("parseFileName", Nil, MString :: MString) implements codegen ($cala, ${
      val fileName = $0
      val forge_extension = "forge"
      val dirName = if(fileName.contains("/")) {
        fileName.substring(0, fileName.lastIndexOf("/") + 1)
      } else { "./" }
      val shortName = if(fileName.contains("/")) {
        fileName.substring(fileName.lastIndexOf("/")+1)
      } else {
        fileName
      }
      if (shortName.size == 0) { println("Directories not currently supported") }
      if(shortName.contains(".")) {
        val extension = shortName.substring(shortName.lastIndexOf(".")+1)
        val newExtension =
          if (extension == forge_extension) forge_extension+"."+forge_extension // wtf
          else forge_extension
        //(dirName, shortName.substring(0, shortName.lastIndexOf(".")) + "." + newExtension)
        shortName.substring(0, shortName.lastIndexOf(".")) + "." + newExtension
      } else {
        //(dirName, shortName + "." + forge_extension)
        shortName + "." + forge_extension
      }
    })

    static (Table) ("apply", Nil, (SSArray, MInt, MInt, MSI, MString) :: Table, effect = mutable) implements allocates(Table, ${$0}, ${$1}, ${$2}, ${$3}, ${$4})

    static (Table) ("apply", Nil, MString :: Table) implements composite ${
      val d = fromFile($0)
      Table(d, array_length(d), array_length(array_apply(d, 0)), map_empty[String, Int](), parseFileName($0))
    }

    direct (Table) ("fromFile", Nil, MString :: SSArray) implements codegen ($cala, ${
      scala.io.Source.fromFile($0).getLines().map(_.split(",").toArray).toArray
    }) 

    // ignore header because
    direct (Table) ("toFile", Nil, (MString, MString, SSArray, MSI) :: MUnit, effect = simple) implements codegen ($cala, ${
      val of = new java.io.PrintStream(new java.io.FileOutputStream($0 + $1))
      of.println($2.map(x => x.mkString(",")).mkString("#"))
      of.close()
    })

  }
}
