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

    val TableOps = withTpe (Table)
    TableOps {                  
      // data fields     
      data(("_data", SSArray), ("_width", MInt) /*, ("_header", Map[String, Int])*/, ("_name", MString))

      // allocation
      op (Table) ("apply", static, Nil, (SSArray, MInt, MString), Table, effect = mutable) implements allocates(Table, ${$0}, ${$1}, ${$2})

      // todo test wrong experiment wip dev how to have defaults
      op (Table) ("apply", static, Nil, (MInt, MString), Table, effect = mutable) implements allocates(Table, ${array_empty[ForgeArray[String]]($0)}, ${$0}, ${$1})
      
      // getters and setters
      "data" is (compiler, Nil, SSArray) implements getter(0, "_data")
      "copy" is (compiler, SSArray, MUnit, effect=write(0)) implements setter(0, "_data", quotedArg(1))
      "length" is (infix, Nil, MInt) implements composite ${array_length(data($self))}
      
      // data ops             
      "apply" is (infix, (MInt, MInt), MString) implements composite ${ array_apply(array_apply(data($self), $1), $2) }                        
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

      /* 
      * OptiWrangler Ops 
      * We are not using the things up there 
      * It is presently necessary infrastructure
      */

      "cutRow" is (infix, (MInt, SArray), SArray) implements composite ${
        array_map[String, String]($2, cell =>
          if($1 >= cell.size) cell
          else cell.substring(0, $1) + cell.substring($1 + 1)
        )
      }
      "cut" is (infix, MInt, Table) implements composite ${
        if($1 < 0) println("Well there goes the __ neighborhood")
        copy($self, array_map[ForgeArray[String], ForgeArray[String]](data($self), row => $self.cutRow($1, row)))
        $self
      }

      // IO - could be better
      "fromFile" is (infix, MString, SSArray) implements codegen ($cala, ${
        scala.io.Source.fromFile($1).getLines().map(_.split(",").toArray).toArray
      }) // tpdo - use split etc. etc.
      "tableFromFile" is (infix, MString, Table) implements single ${ //single?
        val d = $self.fromFile($1)
        Table(d, array_length(d), "")
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
 
