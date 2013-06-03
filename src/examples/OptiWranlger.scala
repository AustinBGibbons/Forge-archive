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
    direct (Clock) ("clock", Nil, MAny :: MInt, effect = simple) implements codegen ($cala, ${
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
      // getters and setters
      compiler ("data") (Nil :: SSArray) implements getter(0, "_data")
      compiler ("header") (Nil :: MSI) implements getter(0, "_header")
      infix ("length") (Nil :: MInt) implements getter(0, "_length")
      compiler ("width") (Nil :: MInt) implements getter(0, "_width")
      compiler ("name") (Nil :: MString) implements getter(0, "_name")
      compiler ("set_data") (SSArray :: MUnit, effect=write(0)) implements setter(0, "_data", quotedArg(1))
      compiler ("set_header") (MSI :: MUnit, effect=write(0)) implements setter(0, "_header", quotedArg(1))
      compiler ("set_length") (MInt :: MUnit, effect=write(0)) implements setter(0, "_length", quotedArg(1))
      compiler ("set_width") (MInt :: MUnit, effect=write(0)) implements setter(0, "_width", quotedArg(1))
      compiler ("set_name") (MString :: MUnit, effect=write(0)) implements setter(0, "_name", quotedArg(1))

      // parallelization - trying this style
      infix ("new_table") (MInt :: Table, effect=mutable) implements allocates (Table, 
        ${array_empty[ForgeArray[String]]($1)}, ${$1}, ${array_length(array_apply(data($0), unit(0)))}, 
          ${map_empty[String, Int]()}, ${name($0)}
      )
      // infix ("length") (Nil :: MInt) implements composite ${ length($self) }
      infix ("apply") (MInt :: SArray) implements composite ${ array_apply(data($self), $1) }
      infix ("update") ((MInt, SArray) :: MUnit, effect=write(0)) implements
        composite ${ array_update(data($self), $1, $2) } 

      parallelize as ParallelCollection(SArray, lookupOp("new_table"), lookupOp("length"), lookupOverloaded("apply", 0), lookupOp("update"))

      // More bones
      infix ("apply") ((MInt, MInt) :: MString) implements composite ${
        array_apply(array_apply(data($self), $1), $2)
      } 
/*
      infix ("putHeader") ((MString, MInt) :: MUnit, effect = write(0)) implements composite ${
        map_put[String, Int](header($self), $1, $2)
      }

      infix ("getHeader") (MString :: MInt) implements composite ${
        map_getOrElse[String, Int](header($self), $1, -1) match {
          case x: Int => x
          case z: Rep[Int] => z
          case _ => {goodbye("failed to match map_get outer") ; unit(-1)}
        }
      }

      infix ("getHeaderIndex") (MString :: MInt) implements composite ${
        if(not(map_contains(header($self), $1))) goodbye("Requested a header which doesn't exist : " + $1)
        map_getOrElse[String, Int](header($self), $1, -1) match {
          case x: Int => x
          case x: Rep[Int] => x
        }
      }

      infix ("getColumn") (MInt :: MInt) implements composite ${$1}
      infix ("getColumn") (MString :: MInt) implements composite ${$self.getHeaderIndex($1)}
  */
      // the biggest todo of them all : best path : "Column" type
      infix ("getColumns") (MAny :: MArray(MInt)) implements composite ${array_range(0, width($self))}

      infix ("mapHelper") ((MString ==> MString, MArray(MInt), MArray(MInt)) :: Table) implements map((SArray, SArray), 0, ${ row => 
        mapBody(row, $1, $2, $3)
      })

      infix ("map") ((MString ==> MString, MAny) :: Table) implements composite ${
        val _width = array_range(0, width($self))   
        val indices = $self.getColumns($2)
        $self.mapHelper($1, _width, indices)
      }

      // Meat
      infix ("cut") (MString :: Table) implements composite ${
        $self.map(_.replaceFirst($1, ""), array_range(0, width($self)))
      }

      infix ("cut") ((MString, MAny) :: Table) implements composite ${
        $self.map(_.replaceFirst($1, ""), $2)
      }

      // IO - user
      infix ("toFile") (MString :: MUnit, effect = simple) implements composite ${
        toFile($1, name($self), data($self), header($self))
      }
    }


    //////////////////////////// Codegen ///////////////////////////////

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

    // Scala generation tests
    direct (Table) ("mapBody", Nil, (SArray, MString ==> MString, MArray(MInt), MArray(MInt)) :: SArray) implements codegen ($cala, ${
      $0.zip($2).map{case(cell, index) => 
        if ($b[3].contains(index)) $b[1](cell)
        else cell
      }
    })

    // I/O
    static (Table) ("apply", Nil, (SSArray, MInt, MInt, MSI, MString) :: Table, effect = mutable) implements allocates(Table, ${$0}, ${$1}, ${$2}, ${$3}, ${$4})

    static (Table) ("apply", Nil, MString :: Table) implements composite ${
      val d = fromFile($0)
      Table(d, array_length(d), array_length(array_apply(d,0)), map_empty[String, Int](), parseFileName($0))
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

    //()
  }
}
