package ppl.dsl.forge
package examples

import core.{ForgeApplication,ForgeApplicationRunner}

object ColumnWranglerDSLRunner extends ForgeApplicationRunner with ColumnWranglerDSL

trait ColumnWranglerDSL extends ForgeApplication with ScalaOps {
  /**
   * The name of your DSL. This is the name that will be used in generated files,
   * package declarations, etc.
   */
  def dslName = "ColumnWrangler"
  
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
    //val T = tpePar("T") 
   
    val Column = tpe("Column") 
    //val SArray = tpeInst(MArray, MString)

    val SArray = tpeInst(MArray, MString)

    val ColumnOps = withTpe (Column)
    ColumnOps {                  
      // data fields     
      data(("_data", SArray), ("_header", MString))

      // allocation
      //op (Column) ("apply", static, Nil, (SArray, MString), Column, effect = mutable) implements allocates(Column, ${$0}, ${$1})

      // getters and setters
      "data" is (compiler, Nil, SArray) implements getter(0, "_data")
      "__set_data" is (infix, (SArray), MUnit, effect=write(0)) implements setter(0, "_data", quotedArg(1))
      "set_data" is (compiler, (SArray), MUnit, effect=write(0)) implements setter(0, "_data", quotedArg(1))
      "xyz" is (infix, MInt, MInt, effect = write(0)) implements single ${
        $1
      }

      op (Column) ("apply", static, Nil, MInt, Column, effect = mutable) implements allocates (Column, ${array_empty[String]($0)}, ${unit("")})
      "length" is (infix, Nil, MInt) implements single ${ array_length(data($self)) }
      "apply" is (infix, MInt, MString) implements composite ${ array_apply(data($self), $1) }
      "update" is (infix, (MInt, MString), MUnit, effect=write(0)) implements composite ${array_update(data($self), $1, $2) }

      parallelize as ParallelCollection(MString, lookupOverloaded("apply", 1), lookup("length"), lookupOverloaded("apply",0), lookup("update"))

      //// Column stuff ////
      // "Why can't I be a compiler?"
      /* ColumnWrangler Ops */
      // effect?
      "cut" is (infix, MInt, Column) implements composite ${
        if($1 < 0) println("well there goes the neighborhood") // why does this return
        set_data($self, array_map[String, String](data($self), cell => 
          if($1 >= cell.size) cell
          else cell.substring(0, $1) + cell.substring($1 + 1)
        ))
        $self
      }
      // effect?
      "delete" is (infix, MInt, Column) implements single ${
        println("Buffer type things not supported")
        $self
      }
      // IO, IO, its off to work we go
      // compile?
      "fromFile" is (infix, MString, SArray) implements codegen ($cala, ${
        scala.io.Source.fromFile($1).getLines().toArray
      })
      "columnFromFile" is (infix, MString, Column) implements single ${
        val d = $self.fromFile($1)
        Column(d)
      }
      op (Column) ("apply", static, Nil, (SArray), Column, effect = mutable) implements allocates(Column, ${$0}, ${unit("")})
    }                    
                                        
    ()    
  }
}
 
