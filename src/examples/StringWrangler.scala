package ppl.dsl.forge
package examples

import core.{ForgeApplication,ForgeApplicationRunner}

object StringWranglerDSLRunner extends ForgeApplicationRunner with StringWranglerDSL

trait StringWranglerDSL extends ForgeApplication with ScalaOps {
  /**
   * The name of your DSL. This is the name that will be used in generated files,
   * package declarations, etc.
   */
  def dslName = "StringWrangler"
  
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
    
    val Column = tpe("Column") 
    //val SArray = tpeInst(MArray, MString)

    val SArray = tpeInst(MArray, MString)

    val ColumnOps = withTpe (Column)
    ColumnOps {                  
      // data fields     
      data(("_data", SArray), ("_length", MInt), ("_header", MString))

      // allocation
      op (Column) ("apply", static, Nil, (MInt), Column, effect = mutable) implements allocates(Column, ${ array_empty[String]($0)}, ${$0}, ${unit("")})
      
      // getters and setters
      "data" is (compiler, Nil, SArray) implements getter(0, "_data")
      "set_data" is (compiler, (SArray), MUnit, effect=write(0)) implements setter(0, "_data", quotedArg(1))
      //"length" is (infix, Nil, MInt) implements composite ${array_length(data($self))}
      "length" is (infix, Nil, MInt) implements getter (0, "_length")
      "set_length" is (compiler, (MInt), MUnit, effect=write(0)) implements setter(0, "_length", quotedArg(1))      

      // data ops             
      "apply" is (infix, (MInt), MString) implements composite ${ array_apply(data($self), $1) }
      "update" is (infix, (("i",MInt),("e",MString)), MUnit, effect = write(0)) implements composite ${
        array_update(data($self), $i, $e)
      }
///*
      "appendable" is (compiler, (MInt, MString), MBoolean) implements single("false")
      "copy" is (compiler, (MInt, Column, MInt, MInt), MUnit, effect = write(2)) implements single ${
        array_copy(data($self), $1, data($2), $3, $4)
      }

      // all for append?
      "append" is (infix, (MInt,MString), MUnit, effect = write(0)) implements single ${
        $self.insert($self.length, $2)
      }
      "insert" is (infix, (MInt,MString), MUnit, effect = write(0)) implements single ${
        insertspace($self,$1,1)
        $self($1) = $2
      }
      "insertspace" is (compiler, (("pos",MInt),("len",MInt)), MUnit, effect = write(0)) implements single ${
        ensureextra($self,$len)
        array_copy(data($self),$pos,data($self),$pos+$len,$self.length-$pos)
        set_length($self,$self.length+$len)
      }
      "ensureextra" is (compiler, ("extra",MInt), MUnit, effect = write(0)) implements single ${
        if (array_length(data($self)) - $self.length < $extra) {
          realloc($self, $self.length+$extra)
        }
      }
      "realloc" is (compiler, ("minLen",MInt), MUnit, effect = write(0)) implements single ${
        var n = Math.max(4, array_length(data($self))*2)
        while (n < $minLen) n = n*2
        val d = array_empty[String](n)
        array_copy(data($self), 0, d, 0, $self.length)
        set_data($self, array_asimmutable(d))
      }
//*/
      // no intuition if one is better than the other
      parallelize as ParallelCollection(MString,
      //parallelize as ParallelCollectionBuffer(MString,
       lookupOverloaded("apply",1),
       lookup("length"),
       lookupOverloaded("apply",0),
       lookup("update")/*,
       lookup("set_length"),
       lookup("appendable"),
       lookup("append"),
       lookup("copy")*/
      )            

        //// Column stuff ////
      // "Why can't I be a compiler?"
      "set_header" is (infix, MString, MUnit, effect = write(0)) implements setter(0, "_header", quotedArg(1))

      /* StringWrangler Ops */
      "promote" is (infix, MInt, Column) implements single ${
        if($1 >= $self.length) println("error: requesting to promote row beyond the wall")
        $self.set_header(array_apply[String](data($self), $1))
        $self.delete($1)
      }

      "demote" is (infix, Nil, Column) implements single${
        $self.set_header(unit(""))
        $self
      }

      "cutHelper" is (infix, (MInt, MString), MString) implements single ${
        if($1 < 0) println("well there goes the neighborhood") // why does this return
        if($1 >= $2.size) $2
        else $2.substring(0, $1) + $2.substring($1 + 1)
      }
      "cut" is (infix, MInt, Column) implements map((MString,MString), 0, ${ e =>
        $self.cutHelper($1, e)
      })
      "cutHelper" is (infix, (MString, MString), MString) implements single ${
        $2.replaceFirst($1, unit(""))
      }
      "cut" is (infix, MString, Column) implements map((MString,MString), 0, ${ e =>
        $self.cutHelper($1, e)
      })
      "cutAllHelper" is (infix, (MString, MString), MString) implements single ${
        $2.replaceAllLiterally($1, unit(""))
      }
      "cutAll" is (infix, MString, Column) implements map((MString,MString), 0, ${ e =>
        $self.cutAllHelper($1, e)
      })
      "cutRightHelper" is (infix, (MString, MString), MString) implements single ${
        val index = $2.lastIndexOf($1)
        if(index == -1) $2
        else $2.substring(0, index) + $2.substring(index + $1.size)
      }
      "cutRight" is (infix, MString, Column) implements map((MString,MString), 0, ${ e =>
        $self.cutRightHelper($1, e)
      })
      "cutHelper" is (infix, (MString ==> MString, MString), MString) implements single ${
        val result = $1($2)
        val index = $2.indexOf(result)
        if(index == -1) $2
        else $2.substring(0, index) + $2.substring(index+result.size)
      }
      "cut" is (infix, MString ==> MString, Column) implements map((MString,MString), 0, ${ e =>
        $self.cutHelper($1, e)
      })
  
      // split extract don't do that thing they do because unzip does not exist 
      // because array^2 does note exist
        //if($1 < o) println("split with negative") 
        // some logic
      "splitHelper" is (infix, (MInt, MString), MString) implements single ${
        $2
      }
      "splitMap" is (infix, MInt, Column) implements map((MString, MString), 0, ${ e =>
        $self.splitHelper($1, e)
      })
      "split2" is (infix, MInt, Column) implements composite ${
        if($1 < 0 ) println("split with negative")
        $self.splitMap($1)
      }
      "delete" is (infix, MInt, Column) implements single ${
        println("Buffer type things not supported")
        $self
      }

      "edit" is (infix, MString ==> MString, Column) implements map((MString, MString), 0, ${ e =>
        $1(e)
      })

      // IO, IO, its off to work we go
      // compile?
      "fromFile" is (infix, MString, SArray) implements codegen ($cala, ${
        scala.io.Source.fromFile($1).getLines().toArray
      })
      "columnFromFile" is (infix, MString, Column) implements single ${
        val d = $self.fromFile($1)
        Column(d, array_length[String](d))
      }
      op (Column) ("apply", static, Nil, (SArray, MInt), Column, effect = mutable) implements allocates(Column, ${$0}, ${$1}, ${unit("")}/*${array_apply($0, unit(0))}*/)
    }                    
                                        
    ()    
  }
}
 
