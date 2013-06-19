package ppl.dsl.forge
package examples

import core.{ForgeApplication,ForgeApplicationRunner}

object CVWranglerDSLRunner extends ForgeApplicationRunner with CVWranglerDSL

trait CV_Base extends ForgeApplication {
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

trait CVWranglerDSL extends CV_Base with SimpleVectorDSL {
  override def dslName = "CVWrangler"
  
  override def specification() = {
    importScalaOps()
    addErrorChecking()
    addClock()
    importVectorOps()
    //addWranglerOps()
    addTableOps()
  }

  def addTableOps() {
    val A = tpePar("A")
    val Table = tpe("Table")
    val Vector = lookupTpe("Vector")
    data(Table, ("_data", Vector(Vector(MString))), ("_length", MInt), ("_width", MInt)/*, ("_header", MSI), ("_name", MString)*/)

    compiler (Table) ("array_contains", Nil, (MArray(MInt), MInt) :: MBoolean) implements single ${
      var i = 0
      var found = false
      //println("array length is: " + array_length($0))
      //println("array(0) is: " + array_apply($0,0))
      //println("looking for elem: " + $1)

      while(i < array_length($0) && !found) {
        if (array_apply($0,i) == $1) found = true
        i += 1        
      //while (primitive2_andand(ordering2_lt(i, array_length($0)), primitive2_unary_bang(found))) {
      //   if (ordering2___equal(array_apply($0,i), $1)) found = true
      //   i += 1
      }
      //if (found == unit(false)) println("did not find elem!")
      //println("found: " + found)
      found
    }

/*
    direct (Table) ("mapHelperHelper", Nil, (("row",Vector(MString)),("func",MString ==> MString),("indices",MArray(MInt))) :: Vector(MString)) implements single ${
      if (row == unit(null)) unit(null.asInstanceOf[ForgeArray[String]])
      else {
        //row
        val out = array_empty[String](array_length($row))
        var i = 0
        while (i < array_length($row)) {
          if (array_contains($indices, i))
            array_update(out,i,$func(array_apply($row,i)))
          else
            array_update(out,i,array_apply($row,i)) 
          i += 1
        }
        out.unsafeImmutable 
      }
    }

    direct (Table) ("filterHelperHelper", Nil, (("row",Vector(MString)),("cond",MString ==> MBoolean),("indices",MArray(MInt))) :: MBoolean) implements single ${
     //!($1(array_apply(row,unit(1))))
     var i = 0
     var found = true
     while (i < array_length($row) && found) {
       if (array_contains($indices,i)) {
         found = !$cond(array_apply($row,i))
        }
        i += 1          
      } 
      found
    }
  */  
    val TableOps = withTpe (Table)
    TableOps {
      // getters and setters
      compiler ("data") (Nil :: Vector(Vector(MString))) implements getter(0, "_data")
      // compiler ("header") (Nil :: MSI) implements getter(0, "_header")
      infix ("length") (Nil :: MInt) implements getter(0, "_length")
      compiler ("width") (Nil :: MInt) implements getter(0, "_width")
      // compiler ("name") (Nil :: MString) implements getter(0, "_name")
      compiler ("set_data") (Vector(Vector(MString)) :: MUnit, effect=write(0)) implements setter(0, "_data", quotedArg(1))
      // compiler ("set_header") (MSI :: MUnit, effect=write(0)) implements setter(0, "_header", quotedArg(1))
      compiler ("set_length") (MInt :: MUnit, effect=write(0)) implements setter(0, "_length", quotedArg(1))
      compiler ("set_width") (MInt :: MUnit, effect=write(0)) implements setter(0, "_width", quotedArg(1))
      // compiler ("set_name") (MString :: MUnit, effect=write(0)) implements setter(0, "_name", quotedArg(1))

      compiler ("update")

      //infix ("getColumns") (MAny :: MArray(MInt)) implements composite ${ 
      infix ("getColumns") (MArray(MInt) :: MArray(MInt)) implements composite ${ 
        //getColumnsBody($1, header($self))
        $1
      }

      infix ("filterHelperHelper") ((Vector(MString), MString ==> MBoolean) :: Vector(MString)) implements composite ${
        $1.filter($2)
      }

      infix ("filterHelper") ((MString ==> MBoolean, MArray(MInt)) :: Table) implements composite ${
        var i = 0   
        //val data = data($self)
        for(i < $self.length) {
          if(array_contains($2, i))
            update($self, $2, $self.filterHelperHelper(col,$1))
        }
        $self
      }
      
      infix ("filter") ((MString ==> MBoolean, MArray(MInt)) :: Table) implements composite ${
        $self.filterHelper($1, $self.getColumns($2))
      }
      
      infix ("delete") ((MString ==> MBoolean, MInt) :: Table) implements composite ${
        val a = array_empty[Int](1)
        a(0) = $2
        val out = $self.filter($1, a.unsafeImmutable)
        println("before filter size: " + $self.length)
        println("after filter size: " + out.length)
        out
      }      
      
      // this is doing numRows (e.g. 3M) allocations, while the c version does 3M element updates.. no comparison
      infix ("mapHelper") ((MString ==> MString, MArray(MInt)) :: Table) implements map((Vector(MString), Vector(MString)), 0, ${ row => {
        mapHelperHelper(row,$1,$2)
      }})
      
      infix ("map") ((MString ==> MString, MArray(MInt)) :: Table) implements composite ${
        // val _width = array_range(0, width($self))   
        val indices = $self.getColumns($2)
        // $self.mapHelper($1, _width, indices)
        $self.mapHelper($1, indices)
      }
      
      infix ("cut") ((MInt, MArray(MInt)) :: Table) implements composite ${
        $self.map(cell => {
          //if ($1 < 0) goodbye("Trying to cut on bad index: " + $1)
          if ($1 >= cell.size) cell
          else cell.substring(0, $1) + cell.substring($1 + 1)//ow_int_plus($1, 1))
        }, $2)
      }

      // Meat
      infix ("cutBefore") ((MInt, MInt) :: Table) implements composite ${
        val a = array_empty[Int](1)
        a(0) = $2
        $self.cut($1, a.unsafeImmutable)
        // $self.cut($1, array($2))
      }
      
      // --- gene app

      ///////////////
        
      infix ("force") (Nil :: Table) implements composite ${
        println($self.length)
        $self
      }
    }

    //////////////////////////// Codegen ///////////////////////////////

    static (Table) ("apply", Nil, (Vector(Vector(MString)), MInt, MInt/*, MSI, MString*/) :: Table, effect = mutable) implements allocates(Table, ${$0}, ${$1}, ${$2}/*, ${$3}, ${$4}*/)
    
    static (Table) ("pure", Nil, (Vector(Vector(MString)), MInt, MInt/*, MSI, MString*/) :: Table) implements allocates(Table, ${$0}, ${$1}, ${$2}/*, ${$3}, ${$4}*/)

    static (Table) ("apply", Nil, MString :: Table) implements single ${
      val lines = ForgeFileReader.readLines($0)(s => s)
      //val d = array_empty[ForgeArray[String]](array_length(lines))
      val width = array_length(array_apply(lines, 0))
      val d = array_empty[ForgeArray[String]](width)//array_length(lines))
      var k = 0
      // allocate
      while (k < width) {
        array_update(d, k, Vector[String](array_length(lines)))
        k += 1
      }
      // assign
      var i = 0
      while (i < array_length(lines)) {
        val line = array_apply(lines, i).fsplit("\t")
        var j = 0
        while (j < width) {
          array_apply(d, j).update(i, array_apply(line, i))
          j += 1
        }
        array_update(d, i)
        //array_update(d, i, array_apply(lines,i).fsplit("\t"))
        i += 1
      }
      val dImm = d.unsafeImmutable
      Table.pure(dImm, array_length(dImm), array_length(array_apply(dImm,0))/*, map_empty[String, Int](), parseFileName($0)*/)
    }
    
    // `Temp`
    direct (Table) ("clip", Nil, MString :: MBoolean) implements codegen ($cala, ${
      val in = $0
      val g = "AGAT"//.toList
      if (in.length < g.length) false
      else {
        var i = 0
        var m = true
        while (i < g.length && m) {
          if (in(i) != g(i) && in(i) != 'N') m = false
          i += 1
        }
        m
      }
    })
  }
}
