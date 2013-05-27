package ppl.dsl.forge
package examples

import core.{ForgeApplication,ForgeApplicationRunner}

object IntVectorDSLRunner extends ForgeApplicationRunner with IntVectorDSL

trait IntVectorDSL extends ForgeApplication with ScalaOps {
  /**
   * The name of your DSL. This is the name that will be used in generated files,
   * package declarations, etc.
   */
  def dslName = "IntVector"
  
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
    addVectorOps()
  }
  
  
  def addVectorOps() {            
    // generic type parameters we will use 
    //val MInt = tpePar("T") 
    val R = tpePar("R")
    
    val Vector = tpe("Vector") 
  
    // data fields     
    data(Vector, ("_length", MInt), ("_data", MArray(MInt)))      
  
    // allocation
    static (Vector) ("apply", Nil, MInt :: Vector, effect = mutable) implements allocates(Vector, ${$0}, ${ array_empty[Int]($0) })
    
    // doesn't rewrite correctly if we use "withTpe (Vector) {", but works if we use:
    val VectorOps = withTpe (Vector)
          
    VectorOps {                                        
      // getters and setters
      compiler ("vector_raw_data") (Nil :: MArray(MInt)) implements getter(0, "_data")
      compiler ("vector_set_raw_data") (MArray(MInt) :: MUnit, effect = write(0)) implements setter(0, "_data", quotedArg(1))
      infix ("length") (Nil :: MInt) implements getter(0, "_length")
      compiler ("vector_set_length") (MInt :: MUnit, effect = write(0)) implements setter(0, "_length", quotedArg(1))      
      
                
      // data ops             
      infix ("apply") (MInt :: MInt) implements composite ${ array_apply(vector_raw_data($self), $1) }                        
      // example named arg
      infix ("update") ((("i",MInt),("e",MInt)) :: MUnit, effect = write(0)) implements composite ${
        array_update(vector_raw_data($self), $i, $e)
      }
      
      // example named, default arg. 'MethodSignature' is currently explicitly needed when mixing arg types.    
      infix ("slice") (MethodSignature(List(("start",MInt,"0"),("end",MInt)), Vector)) implements single ${
        val out = Vector($end - $start)
        var i = $start
        while (i < $end) {
          out(i-$start) = $self(i)
          i += 1
        }
        out
      }        
                
      infix ("insert") ((MInt,MInt) :: MUnit, effect = write(0)) implements single ${
        vector_insertspace($self,$1,1)
        $self($1) = $2
      }
      
      infix ("append") ((MInt,MInt) :: MUnit, effect = write(0)) implements single ${
        $self.insert($self.length, $2)
      }        
      
      compiler ("vector_insertspace") ((("pos",MInt),("len",MInt)) :: MUnit, effect = write(0)) implements single ${
        vector_ensureextra($self,$len)
        val data = vector_raw_data($self)
        array_copy(data,$pos,data,$pos+$len,$self.length-$pos)
        vector_set_length($self,$self.length+$len)
      }
      
      compiler ("vector_ensureextra") (("extra",MInt) :: MUnit, effect = write(0)) implements single ${
        val data = vector_raw_data($self)
        if (array_length(data) - $self.length < $extra) {
          vector_realloc($self, $self.length+$extra)
        }
      }
      
      compiler ("vector_realloc") (("minLen",MInt) :: MUnit, effect = write(0)) implements single ${
        val data = vector_raw_data($self)
        var n = Math.max(4, array_length(data)*2)
        while (n < $minLen) n = n*2
        val d = array_empty[Int](n)
        array_copy(data, 0, d, 0, $self.length)
        vector_set_raw_data($self, array_asimmutable(d))
      }        
      
      infix ("zero") (Nil :: MInt) implements single ${0}
      
      // math      
      infix ("+") (Vector :: Vector) implements zip((MInt,MInt,MInt), (0,1), ${ (a,b) => a+b })
      infix ("*") (MInt :: Vector) implements map((MInt,MInt), 0, "e => e*"+quotedArg(1))      
      infix ("sum") (Nil :: MInt) implements reduce((MInt,Vector), 0, lookup("Vector", "zero"), ${ (a,b) => a+b })
             
      // bulk        
      infix ("map") ((MInt ==> MInt) :: Vector) implements map((MInt,MInt), 0, ${ e => $1(e) })
      
      infix ("reduce") (((MInt,MInt) ==> MInt) :: MInt) implements reduce((MInt,Vector), 0, lookup("Vector", "zero"), ${
        (a,b) => $1(a,b)
      })
      
      infix ("filter") ((MInt ==> MBoolean) :: Vector) implements filter((MInt,MInt), 0, ${e => $1(e)}, ${e => e})
      
      infix ("mapreduce") ((MInt ==> MInt,(MInt,MInt) ==> MInt) :: MInt) implements composite ${
        $self.map($1).reduce($2)
      }
      
      
      // misc      
      // will print out of order in parallel, but hey
      infix ("pprint") (Nil :: MUnit, effect = simple) implements foreach((MInt,Vector), 0, ${a => println(a)}) 
      
            
      // parallel collectionification
      // This enables a tpe to be passed in as the collection type of a Delite op      
      compiler ("vector_appendable") ((MInt,MInt) :: MBoolean) implements single("true")
      compiler ("vector_copy") ((MInt,Vector,MInt,MInt) :: MUnit, effect = write(2)) implements single ${
        val src = vector_raw_data($self)
        val dest = vector_raw_data($2)
        array_copy(src, $1, dest, $3, $4)
      }

      parallelize as ParallelCollectionBuffer(MInt, lookupOverloaded("apply",1), lookup("length"), lookupOverloaded("apply",0), lookup("update"), lookup("vector_set_length"), lookup("vector_appendable"), lookup("append"), lookup("vector_copy"))            
    }                    
  
    ()    
  }
}
 
