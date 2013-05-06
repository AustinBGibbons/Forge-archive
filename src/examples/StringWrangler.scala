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
      data(("_data", SArray), ("_width", MInt) /*, ("_header", Map[String, Int])*/, ("_name", MString))

      // allocation
      // todo test wrong experiment wip dev how to have defaults
      op (Column) ("apply", static, Nil, (MInt, MString), Column, effect = mutable) implements allocates(Column, ${ array_empty[String]($0)}, ${$0}, ${$1})
      
      // getters and setters
      "data" is (compiler, Nil, SArray) implements getter(0, "_data")
      "length" is (infix, Nil, MInt) implements composite ${array_length(data($self))}
      
      // data ops             
      "apply" is (infix, (MInt), MString) implements composite ${ array_apply(data($self), $1) }                        
      // example named arg
      "update" is (infix, (("i",MInt),("e",MString)), MUnit, effect = write(0)) implements composite ${
        array_update(data($self), $i, $e)
      }

      parallelize as ParallelCollection(MString,
       lookupOverloaded("apply",1),
       lookup("length"),
       lookupOverloaded("apply",0),
       lookup("update") 
      )            

      /* StringWrangler Ops */
/*
      "copy" is (compiler, (SSArray), Column) implements setter(0, "_data", quotedArg(1)) 

  //"mapWrap" is (compiler, (MString ==> MString, MInt), SSArray) implements map((SArray, SArray), 0, 
  //${ e => $1(array_apply(data($self), $2)) }
      "mapWrap" is (compiler, (MString, MInt), SSArray) implements map((SArray, SArray), 0, 
        ${ e => $e }
      )
  */    
/*
      "map" is (compiler, (MString ==> MString, MInt), Column) implements composite ${copy(mapWrap($0, $1, $2))}
*/
      //"cut" is (infix, (MString, MInt), Column) implements composite ${ map(_.replaceFirst($1, $2)) }

      //"potato" is (compiler, (MInt), MUnit) implements single ${}
    }                    
                                        
    ()    
  }
}
 
