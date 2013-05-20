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
    // val T = tpePar("T") 
    // val R = tpePar("R")
    
    val Table = tpe("Table") 
    //val SArray = tpeInst(MArray, MString)

    val SArray = tpeInst(MArray, MString)
    val SSArray = tpeInst(MArray, SArray)
    val MSI = tpeInst(MMap, List(MString, MInt))

      data(Table, ("_data", SSArray), ("_width", MInt), ("_header", MSI), ("_name", MString))
      // allocation
      //op (Table) ("apply", static, Nil, (SSArray, MInt, MSI, MString) :: Table, effect = mutable) implements allocates(Table, ${$0}, ${$1}, ${$2}, ${$3})
      op (Table) ("apply", static, Nil, (SSArray, MInt, MSI, MString) :: Table, effect = mutable) implements allocates(Table, ${$0}, ${$1}, ${$2}, ${$3})

      // todo test wrong experiment wip dev how to have defaults
      op (Table) ("apply", static, Nil, (MInt, MString) :: Table, effect = mutable) implements allocates(Table, ${array_empty[ForgeArray[String]]($0)}, ${$0}, ${map_empty[String, Int]()}, ${$1})

    // should be a set etc. etc.  todo temp fixme dome etc.
    /*
    "contains" is (direct, (MArray(MInt), MInt) :: MBoolean) implements codegen ($cala, ${
      $1.contains($2)
    })
    */


    // static?
    op (Table) ("fromFile", direct, Nil, MString :: SSArray) implements codegen ($cala, ${
      scala.io.Source.fromFile($0).getLines().map(_.split(",").toArray).toArray
    }) // tpdo - use split etc. etc.

    op (Table) ("contains", direct, Nil, (MArray(MInt), MInt) :: MBoolean) implements codegen ($cala, ${
      $0.contains($1)
    })

    // maybe allocates?
    op (Table) ("array", direct, Nil, MString :: SArray) implements codegen ($cala, ${
      Array($0)
    })

    op (Table) ("array", direct, Nil, (MString, MString) :: SArray) implements codegen ($cala, ${
      //println("split: " + $0 + " " + $1)
      Array($0, $1)
    })

    val TableOps = withTpe (Table)
    TableOps {                  
      
      // getters and setters
      "data" is (compiler, Nil :: SSArray) implements getter(0, "_data")
      "header" is (compiler, Nil :: MSI) implements getter(0, "_header")
      "width" is (compiler, Nil :: MInt) implements getter(0, "_width")
      "set_data" is (compiler, SSArray :: MUnit, effect=write(0)) implements setter(0, "_data", quotedArg(1))
      "length" is (infix, Nil :: MInt) implements composite ${array_length(data($self))}
      
      // data ops             
      "apply" is (infix, (MInt, MInt) :: MString) implements composite ${ array_apply(array_apply(data($self), $1), $2) }                        
      "apply" is (infix, (MInt) :: SArray) implements composite ${ array_apply(data($self), $1) }                        
      // example named arg
      "update" is (infix, (("i",MInt),("e",SArray)) :: MUnit, effect = write(0)) implements composite ${
        array_update(data($self), $i, $e)
      }

      parallelize as ParallelCollection(SArray,
       lookupOverloaded("apply",1),
       lookup("length"),
       lookupOverloaded("apply",0),
       lookup("update") 
      )            

      /* 
      * OptiWrangler Ops 
      * We are not using the things up there 
      * It is presently necessary infrastructure
      */

      /* 
        just testing
      */

///*
      "putHeader" is (infix, (MString, MInt) :: MUnit, effect = write(0)) implements composite ${
        map_put[String, Int](header($self), $1, $2)
      }
//*/

///*
      "getHeader" is (infix, MString :: MInt) implements composite ${
        map_getOrElse[String, Int](header($self), $1, -1) match {
//*/
          /*
          case Def(p) => p match {
            //case q: MInt=> {println("found a MInt") ; q}
            case r: Int=> {println("lifting with unit") ; unit(r)}
            case _ => {println("failed to match map_get outer") ; unit(-1)}
          }
          */
///*
          case x: Int => {println("found an int") ; x}
          //case y: MInt => {println("found an mint") ; y}
          case z: Rep[Int] => {println("found an rep[int]") ; z}
          //case a: Rep[MInt] => {println("found an rep[mint]") ; a}
          //case y: Def(z) => {println("Found somethinf in a def()") ; z}
          case _ => {println("failed to match map_get outer") ; unit(-1)}
        }
      }
//*/

      // Friends and Helpers

      "map" is (infix, (MString ==> MString, MAny) :: Table) implements composite ${
        val indices = array_range(0, width($self)) //array_range(0, 1) //getColumns($2)
// this line intentionally left blank
        //set_data($self, array_map[ForgeArray[String], ForgeArray[String]](data($self), row => array_zipwith[String, Int, String](row, array_range(0, width($self)), (cell, index) =>
        set_data($self, array_map[ForgeArray[String], ForgeArray[String]](data($self), row => array_zipwith[String, Int, String](row, indices, (cell, index) =>
          if(contains(indices, index)) $1(cell) 
          else cell
        )))
        $self // perhaps fold into set_data
      }

      "flatMap" is (infix, (MString ==> SArray, MAny) :: Table) implements composite ${
        val indices = array_range(0, 1) //getColumns($2)
// this line intentionally left blank
        set_data($self, array_flatmap[ForgeArray[String], ForgeArray[String]](data($self), row => array_zipwith[String, Int, ForgeArray[String]](row, indices, (cell, index) =>
          if(contains(indices, index)) $1(cell)
          else array(cell)
        )))
        $self // perhaps fold into set_data
      }

      // API // note there is no error handling

      // CUT

      "cut" is (infix, (MInt, MAny) :: Table) implements composite ${
        if($1 < 0) println("Trying to cut on index: " + $1)
        $self.map((cell => {
          if ($1 >= cell.size) cell  
          else cell.substring(0, $1) + cell.substring($1 + 1)
        }), $2)
      }

      "cut" is (infix, (MString, MAny) :: Table) implements composite ${
        $self.map(_.replaceFirst($1, ""), $2)
      }
    
      "cut" is (infix, (MString ==> MString, MAny) :: Table) implements composite ${
        $self.map((cell => {
          val result = $1(cell)
          val index = cell.indexOf(result)
          if(index eq -1) cell
          else cell.substring(0, index) + cell.substring(index+result.size)
        }), $2)
      }

      "cutRight" is (infix, (MString, MAny) :: Table) implements composite ${
        $self.map((cell => {
          val index = cell.lastIndexOf($1)
          if(index eq -1) cell
          else cell.substring(0, index) + cell.substring(index + $1.size)
        }), $2)
      }

      "cutAll" is (infix, (MString, MAny) :: Table) implements composite ${
        $self.map(_.replaceAllLiterally($1, ""), $2)
      }

      // SPLIT
      
      "split" is (infix, (MInt, MAny) :: Table) implements composite ${
        if($1 < 0) println("Trying to split on index: " + $1)
        $self.flatMap((cell => {
          if ($1 >= cell.size) array(cell, "")
          else array(cell.substring(0, $1), cell.substring($1 + 1)) // this is wrong... but why
        }), $2)
      }

      "split" is (infix, (MString, MAny) :: Table) implements composite ${
        $self.flatMap((cell => {
          val index = cell.indexOf($1)
          if (index eq -1) array(cell, "")
          else array(cell.substring(0, index), cell.substring(index+$1.size))
        }), $2)
      }

      "split" is (infix, (MString ==> MString, MAny) :: Table) implements composite ${
        $self.flatMap((cell => {
          val result = $1(cell)
          val index = cell.indexOf(result)
          if (index eq -1) array(cell, "")
          else array(cell.substring(0, index), cell.substring(index+result.size))
        }), $2)
      }

      "splitRight" is (infix, (MString, MAny) :: Table) implements composite ${
        $self.flatMap((cell => {
          val index = cell.lastIndexOf($1)
          if (index eq -1) array(cell, "")
          else array(cell.substring(0, index), cell.substring(index+$1.size))
        }), $2)
      }

      "splitAll" is (infix, (MString, MAny) :: Table) implements composite ${
        $self.flatMap(_.split($1), $2)
      }

      // EXTRACT
      "extract" is (infix, (MInt, MAny) :: Table) implements composite ${
        if($1 < 0) println("Trying to extract on index: " + $1)
        $self.flatMap(cell => {
          if($1 >= cell.size) array(cell, "")
          else array(cell, cell.xcharAt($1))
        }, $2)
      }

      "extract" is (infix, (MString, MAny) :: Table) implements composite ${
        $self.flatMap(cell => {
          if(cell.indexOf($1) eq -1) array(cell, "")
          else array(cell, $1)
        }, $2)
      }

      "extract" is (infix, (MString ==> MString, MAny) :: Table) implements composite ${
        $self.flatMap(cell => {
          val result = $1(cell)
          if(cell.indexOf(result) eq -1) array(cell, "")
          else array(cell, result)
          array(cell, cell)
        }, $2)
      }

      // EDIT : just wraps map at the moment 
      "edit" is (infix, (MString ==> MString, MAny) :: Table) implements composite ${
        $self.map($1, $2)
      }

      // DELETE - unsupported
      "delete" is (infix, (MInt) :: Table) implements composite ${
        println("Delete not currently supported")
        $self
      }

      // (wrap to filter) todododododotodo
      "delete" is (infix, (MString ==> MBoolean) :: Table) implements composite ${
        println("Delete not currently supported")
        $self
      }

      // DROP - todo reforge
      "drop" is (infix, MInt :: Table) implements composite ${
        println("Drop not currently supported")
        $self
      }

      "drop" is (infix, MString :: Table) implements composite ${
        println("Drop not currently supported")
        $self
      }
    
    /*
      "drop" is (infix, Seq[Any] :: Table) implements composite ${
        println("Drop not currently supported")
        $self
      }
    */
    
      // MERGE

      // TRANSPOSE

      // TRANSLATE

      // FOLD

      // UNFOLD

      // IO - could be better
      "tableFromFile" is (infix, MString :: Table) implements composite ${ 
        val d = fromFile($1)
        Table(d, array_length(d), map_empty[String, Int](), "")
      }


      /*
      op (Table) ("apply", static, Nil, (MString), Table, effect=mutable) implements composite ${
        val d = fromFile($1)
        Table(d, array_length(d), unit(""))
      }
      */
    }                    
 
    ()    
  }
}
 
