package ppl.dsl.forge
package examples

import core.{ForgeApplication,ForgeApplicationRunner}

object SSOWDSLRunner extends ForgeApplicationRunner with SSOWDSL

trait SSOWDSL extends ForgeApplication {
  /**
   * The name of your DSL. This is the name that will be used in generated files,
   * package declarations, etc.
   */
  def dslName = "SSOW"
  
  /**
   * The specification is the DSL definition (types, data structures, ops)
   */
  def specification() = {
    /**
     * Include Scala ops
     */
    importScalaOps() 
    
    /**
     * The main portion of our DSL
     */        
    importVectorOps()    
  }
  
  
  def importVectorOps() {            
    // generic type parameters we will use 
    val T = tpePar("T") 
    val R = tpePar("R")
    
    val Vector = tpe("Vector", T) 
  
    // data fields     
    data(Vector, ("_length", MInt), ("_data", MArray(T)))      
 
    direct (Vector) ("clock", Nil, MUnit :: MInt, effect = simple) implements codegen ($cala, ${
      System.currentTimeMillis().toInt
    })
 
    direct (Vector) ("clock", Nil, MAny :: MInt, effect = simple) implements codegen ($cala, ${
      System.currentTimeMillis().toInt
    })
 
    // allocation
    static (Vector) ("apply", T, MInt :: Vector(T), effect = mutable) implements allocates(Vector, ${$0}, ${ array_empty[T]($0) })
    
    static (Vector) ("apply", T, (MInt, MArray(T)) :: Vector(T), effect = mutable) implements allocates(Vector, ${$0}, ${$1})

    static (Vector) ("apply", Nil, MString :: Vector(Vector(MString)), effect = mutable) implements composite ${
      val f = fromFile($0)
      Vector[Vector[String]](array_length(f), array_map[ForgeArray[String], Vector[String]](f, x => Vector[String](array_length(x), x)))
    }

    direct (Vector) ("fromFile", Nil, MString :: MArray(MArray(MString))) implements codegen ($cala, ${scala.io.Source.fromFile($0).getLines.map(x => x.split(",")).toArray})

    // doesn't rewrite correctly if we use "withTpe (Vector) {", but works if we use:
    val VectorOps = withTpe (Vector)
          
    VectorOps {                                        
      // getters and setters
      compiler ("vector_raw_data") (Nil :: MArray(T)) implements getter(0, "_data")
      compiler ("vector_set_raw_data") (MArray(T) :: MUnit, effect = write(0)) implements setter(0, "_data", quotedArg(1))
      infix ("length") (Nil :: MInt) implements getter(0, "_length")
      compiler ("vector_set_length") (MInt :: MUnit, effect = write(0)) implements setter(0, "_length", quotedArg(1))      
      
                
      // data ops             
      infix ("apply") (MInt :: T) implements composite ${ array_apply(vector_raw_data($self), $1) }                        
      // example named arg
      infix ("update") ((("i",MInt),("e",T)) :: MUnit, effect = write(0)) implements composite ${
        array_update(vector_raw_data($self), $i, $e)
      }
      
      // example named, default arg. 'MethodSignature' is currently explicitly needed when mixing arg types.    
      infix ("slice") (MethodSignature(List(("start",MInt,"0"),("end",MInt)), Vector(T))) implements single ${
        val out = Vector[T]($end - $start)
        var i = $start
        while (i < $end) {
          out(i-$start) = $self(i)
          i += 1
        }
        out
      }        
                
      infix ("insert") ((MInt,T) :: MUnit, effect = write(0)) implements single ${
        vector_insertspace($self,$1,1)
        $self($1) = $2
      }
      
      infix ("append") ((MInt,T) :: MUnit, effect = write(0)) implements single ${
        $self.insert($self.length, $2)
      }        
      
      infix ("drop") (MInt :: Vector(T), effect = write(0)) implements single ${
        val data = vector_raw_data($self)
        val size = $self.length - ($1 + 1)
        array_copy(data, $1, data, $1+1, size)
        vector_set_length($self, $self.length-1)  
        $self
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
        val d = array_empty[T](n)
        array_copy(data, 0, d, 0, $self.length)
        vector_set_raw_data($self, d.unsafeImmutable)
      }        
      
      
      // math      
      infix ("+") (Vector(T) :: Vector(T), TNumeric(T)) implements zip((T,T,T), (0,1), ${ (a,b) => a+b })
      infix ("*") (T :: Vector(T), TNumeric(T)) implements map((T,T), 0, "e => e*"+quotedArg(1))      
      infix ("sum") (Nil :: T, TNumeric(T)) implements reduce(T, 0, lookupOp("Numeric","zero"), ${ (a,b) => a+b })
             
      // bulk        
      infix ("map") ((T ==> R) :: Vector(R), addTpePars = R) implements map((T,R), 0, ${ e => $1(e) })
      
      infix ("reduce") (((T,T) ==> T) :: T, TNumeric(T)) implements reduce(T, 0, lookupOp("Numeric","zero"), ${
        (a,b) => $1(a,b)
      })
      
      infix ("filter") ((T ==> MBoolean) :: Vector(T)) implements filter((T,T), 0, ${e => $1(e)}, ${e => e})
      
      infix ("mapreduce") ((T ==> T,(T,T) ==> T) :: T, TNumeric(T)) implements composite ${
        $self.map($1).reduce($2)
      }
      
            
      // misc      
      // will print out of order in parallel, but hey
      infix ("pprint") (Nil :: MUnit, effect = simple) implements foreach(T, 0, ${a => println(a)}) 
      
            
      // parallel collectionification
      // This enables a tpe to be passed in as the collection type of a Delite op      
      
      // by convention, the return tpe of alloc must be its last tpe parameter, if it has any
      compiler ("vector_raw_alloc") (MInt :: Vector(R), addTpePars = R) implements single ${
        Vector[R]($1)
      }      
      compiler ("vector_appendable") ((MInt,T) :: MBoolean) implements single("true")
      compiler ("vector_copy") ((MInt,Vector(T),MInt,MInt) :: MUnit, effect = write(2)) implements single ${
        val src = vector_raw_data($self)
        val dest = vector_raw_data($2)
        array_copy(src, $1, dest, $3, $4)
      }

      parallelize as ParallelCollectionBuffer(T, lookupOp("vector_raw_alloc"), lookupOp("length"), lookupOverloaded("apply",3), lookupOp("update"), lookupOp("vector_set_length"), lookupOp("vector_appendable"), lookupOp("append"), lookupOp("vector_copy"))            
    } 
    
    ()    
  }
}
 
