package ppl.dsl.forge
package examples

import core.{ForgeApplication,ForgeApplicationRunner}

object OptiWranglerDSLRunner extends ForgeApplicationRunner with OptiWranglerDSL

trait Base extends ForgeApplication {
  def addErrorChecking() {
    val Error = grp("Error")
    
    // options = MSS ...
/*
    // why static instead of direct
    direct (Error) ("error", Nil, MString :: MUnit, effect = simple) implements codegen ($cala, ${
      val message = $0
      val onError = "fail" //options.getOrElse("onError", "fail")
      onError match {
        case "ignore" =>
        case "warn" => warn(message)
        case "fail" => goodbye("Error: " + message)
      }
    })

    direct (Error) ("warn", Nil, MString :: MUnit, effect = simple) implements codegen ($cala, ${
      val message = $0
      val warnings = "one" //options.getOrElse("warnings", "one")
      warnings match {
        case "none" =>
        //case "one" => {println("\n\n\tWarning: " + message + "\n\n") ; options.put("warnings", "none")}
        case "one" => {println("Warning: " + message + "")}
        case "all" => println("Warning: " + message)
      }  
    })

*/
    direct (Error) ("goodbye", Nil, MString :: MUnit, effect = simple) implements codegen ($cala, ${
      //System.err.println("\n\n\t" + $0 + "\n\n")
      System.err.println("\t" + $0 + "")
      System.exit(-1)
    })
  }
}

trait OptiWranglerDSL extends Base {
  /**
   * The name of your DSL. This the name that will be used in generated files,
   * package declarations, etc.
   */
  def dslName = "OptiWrangler"
  
  /**
   * The specification the DSL definition (types, data structures, ops)
   */
  def specification() = {
    /**
     * Include Scala ops
     */
    importScalaOps() 
    
    addErrorChecking()

    /**
     * The main portion of our DSL
     */        
    addWranglerOps()
  }
  
  
  def addWranglerOps() {            
    // generic type parameters we will use 
    //val T = tpePar("T") 
    // val R = tpePar("R")
    
    val Table = tpe("Table") 
    //val SArray = tpeInst(MArray, MString)

    val SArray = tpeInst(MArray, MString)
    val SSArray = tpeInst(MArray, SArray)
    val MSI = tpeInst(MMap, List(MString, MInt))

    data(Table, ("_data", SSArray), ("_width", MInt), ("_header", MSI), ("_name", MString))
    // allocation

    /*
    static (Table) ("apply", Nil, MInt :: Table, effect = mutable) implements allocates (Table, 
      ${array_empty[ForgeArray[String]]($1)}, ${$1}, ${map_empty[String, Int]()}, ${unit("bubblegum")}
    )
    */

    direct (Table) ("map_body", Nil, (MArray(MString), MArray(MInt), MArray(MInt), MString ==> MString, (MArray(MInt), MInt) ==> MBoolean) :: MArray(MString)) implements codegen ($cala, ${
      $0.zip($1).map{case(cell, index) =>
        if($b[4]($2, index)) $b[3](cell)
        else cell
        //cell
      }
    })

    static (Table) ("apply", Nil, (Table, MInt) :: Table, effect = mutable) implements allocates (Table, 
      ${array_empty[ForgeArray[String]]($1)}, ${$1}, ${map_empty[String, Int]()}, ${unit("bubblegum")}
    )

    static (Table) ("apply", Nil, (SSArray, MInt, MSI, MString) :: Table, effect = mutable) implements allocates(Table, ${$0}, ${$1}, ${$2}, ${$3})

    // todo test wrong experiment wip dev how to have defaults
    static (Table) ("apply", Nil, (MInt, MString) :: Table, effect = mutable) implements allocates(Table, ${array_empty[ForgeArray[String]]($0)}, ${$0}, ${map_empty[String, Int]()}, ${$1})

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

    direct (Table) ("dropCell", Nil, (SArray, MInt) :: SArray) implements codegen ($cala, ${
      $0.take($1) ++ $0.drop($1 + 1)
    })

    direct (Table) ("regexMatch", Nil, (MString, MString) :: MBoolean) implements codegen ($cala, ${
      $0.r.findFirstIn($1) match {
        case None => false
        case _ => true
      }
    })

    //sigh
    direct (Table) ("or", Nil, (MBoolean, MBoolean) :: MBoolean) implements codegen ($cala, ${$0 || $1})

    // static?
    direct (Table) ("fromFile", Nil, MString :: SSArray) implements codegen ($cala, ${
      scala.io.Source.fromFile($0).getLines().map(_.split(",").toArray).toArray
    }) // tpdo - use split etc. etc.

    // ignore header because
    direct (Table) ("toFile", Nil, (MString, MString, SSArray, MSI) :: MUnit, effect = simple) implements codegen ($cala, ${
      val of = new java.io.PrintStream(new java.io.FileOutputStream($0 + $1))
      //of.println($2.map(x => x.mkString(",")).mkString("\\n"))
      of.println($2.map(x => x.mkString(",")).mkString("#"))
      of.close()
    })

    direct (Table) ("contains", Nil, (MArray(MInt), MInt) :: MBoolean) implements codegen ($cala, ${
      $0.contains($1)
    })

    // maybe allocates?
    direct (Table) ("array", Nil, MString :: SArray) implements codegen ($cala, ${
      Array($0)
    })

    direct (Table) ("array", Nil, (MString, MString) :: SArray) implements codegen ($cala, ${
      //println("split: " + $0 + " " + $1)
      Array($0, $1)
    })

////////////////////////////////////////////////////////////////////////
    val TableOps = withTpe (Table)
    TableOps {                  
      
      // getters and setters
      compiler ("data") (Nil :: SSArray) implements getter(0, "_data")
      compiler ("header") (Nil :: MSI) implements getter(0, "_header")
      compiler ("width") (Nil :: MInt) implements getter(0, "_width")
      compiler ("name") (Nil :: MString) implements getter(0, "_name")
      compiler ("set_data") (SSArray :: MUnit, effect=write(0)) implements setter(0, "_data", quotedArg(1))
      compiler ("set_header") (MSI :: MUnit, effect=write(0)) implements setter(0, "_header", quotedArg(1))
      compiler ("set_width") (MInt :: MUnit, effect=write(0)) implements setter(0, "_width", quotedArg(1))
      compiler ("set_name") (MString :: MUnit, effect=write(0)) implements setter(0, "_name", quotedArg(1))
      infix ("length") (Nil :: MInt) implements composite ${array_length(data($self))}
      
      // data ops             
      infix ("apply") ((MInt, MInt) :: MString) implements composite ${ array_apply(array_apply(data($self), $1), $2) }                        
      infix ("apply") ((MInt) :: SArray) implements composite ${ array_apply(data($self), $1) }
 
/*
      infix ("apply") ((MInt) :: Table, effect = mutable) implements allocates (Table, 
        ${array_empty[ForgeArray[String]]($0)}, ${$0}, ${map_empty[String, Int]()}, ${unit("bubblegum")})
*/

      // example named arg
      infix ("update") ((("i",MInt),("e",SArray)) :: MUnit, effect = write(0)) implements composite ${
        array_update(data($self), $i, $e)
      }

      infix ("composite") ((MInt,SArray) :: MUnit, effect = write(0)) implements composite ${
        compositespace($self,$1,1)
        $self($1) = $2
      }

      infix ("append") ((MInt,SArray) :: MUnit, effect = write(0)) implements composite ${
        $self.composite(width($self), $2)
      }

      compiler ("compositespace") ((("pos",MInt),("len",MInt)) :: MUnit, effect = write(0)) implements composite ${
        ensureextra($self,$len)
        array_copy(data($self),$pos,data($self),$pos+$len,width($self)-$pos)
        set_width($self,width($self)+$len)
      }

      compiler ("ensureextra") (("extra",MInt) :: MUnit, effect = write(0)) implements composite ${
        if (array_length(data($self)) - width($self) < $extra) {
          realloc($self, width($self)+$extra)
        }
      }

      compiler ("realloc") (("minLen",MInt) :: MUnit, effect = write(0)) implements composite ${
        var n = Math.max(4, array_length(data($self))*2)
        while (n < $minLen) n = n*2
        val d = array_empty[ForgeArray[String]](n)
        array_copy(data($self), 0, d, 0, width($self))
        set_data($self, d.unsafeImmutable)
      }

      compiler ("table_appendable") ((MInt,SArray) :: MBoolean) implements single("true")
      compiler ("copy") ((MInt,Table,MInt,MInt) :: MUnit, effect = write(2)) implements composite ${
        val src = data($self)
        val dest = data($2)
        array_copy(src, $1, dest, $3, $4)
      }

      parallelize as ParallelCollection(SArray,
       lookupOverloaded("apply",0),
       lookupOp("length"),
       lookupOverloaded("apply",4),
       lookupOp("update")
      )

/*
      parallelize as ParallelCollectionBuffer(SArray,
       lookupOverloaded("apply",4),
       lookup("length"),
       lookupOverloaded("apply",0),
       lookup("update"),
       lookup("set_width"),
       lookup("table_appendable"),
       lookup("append"),
       lookup("copy")
      )            
*/

      /* 
      * OptiWrangler Ops 
      * We are not using the things up there 
      * It presently necessary infrastructure
      */

      infix ("cleanup") (Table :: Table) implements composite ${
        set_name($1, name($self))
        set_header($1, header($self))
        $1
        //$self.set_data(data($1))
      }

      /* 
        just testing
      */

      infix ("putHeader") ((MString, MInt) :: MUnit, effect = write(0)) implements composite ${
        map_put[String, Int](header($self), $1, $2)
      }

      infix ("getHeader") (MString :: MInt) implements composite ${
        map_getOrElse[String, Int](header($self), $1, -1) match {
          case x: Int => {/*println("found an int") ;*/ x}
          case z: Rep[Int] => {/*println("found an rep[int]") ;*/z}
          case _ => {goodbye("failed to match map_get outer") ; unit(-1)}
        }
      }

      // Friends and Helpers

      infix ("getHeaderIndex") (MString :: MInt) implements composite ${
        if(not(map_contains(header($self), $1))) goodbye("Requested a header which doesn't exist : " + $1)
        map_getOrElse[String, Int](header($self), $1, -1) match {
          case x: Int => x
          case x: Rep[Int] => x
        }
      }
  
      // so hacky
      //val T = tpePar("T")
      /*
      infix ("getColumn") (T :: MInt, addTpePars = List(T)) implements composite ${
        val mInt = manifest[Int]
        val mStr = manifest[String]
        manifest[T] match {
          case `mInt` => $1
          case `mStr` => $self.getHeaderIndex($1)
      */
        /*
          case x: MInt => x
          case x: MString => $self.getHeaderIndex(x)
          case x: Int => {println("getColumn on Int") ; x}
          case x: String => {println("getColumn on String") ; $self.getHeaderIndex(x)}
          case x: Rep[Int] => {println("getColumn on Rep[Int]") ; x}
          case x: Rep[String] => {println("getColumn on Rep[String]") ; $self.getHeaderIndex(x)}
        */
      /*
        }
      }
      */

      // honey badger don't care
      infix ("getColumn") (MInt :: MInt) implements composite ${$1}
      infix ("getColumn") (MString :: MInt) implements composite ${$self.getHeaderIndex($1)}
      // the biggest todo of them all : best path : "Column" type
      infix ("getColumns") (MAny :: MArray(MInt)) implements composite ${array_range(0, width($self))}

      // todo toMap
      // hacky
      infix ("indexMap") (SArray :: MSI) implements composite ${
        val r = map_empty[String, Int]()
        array_zipwith[String, Int, Unit]($1, array_range(0, width($self)), (x, y) => map_put(r, x,y))
        r
      }
      infix ("promote") (SArray :: Table) implements composite ${
        set_header($self, $self.indexMap($1))
        $self
      }
  
      infix ("promote") (MInt :: Table) implements composite ${
        if($1 ge width($self)) 
          goodbye("Trying to promote : " + $1 + " but width is : " + width($self))
        set_header($self, $self.indexMap(data($self).apply($1)))
        //delete
        $self
      }

      infix ("demote") (MInt :: Table) implements composite ${
        goodbye("demote not supported")
        $self
      }

      /*
      infix ("filter") ((MArray(MInt), MArray(MInt), MString ==> MBoolean, (MArray(MInt), MInt) ==> MBoolean, (MBoolean, MBoolean) ==> MBoolean) :: Table) implements filter ((SArray, SArray), 0, ${ row =>
          array_reduce[Boolean](array_zipwith[String, Int, Boolean](row, $1, (cell, index) =>
            if($4($2, index)) $3(cell)
            else unit(true)
          ), {(x,y) => $5(x,y)}, unit(false))
        }, ${e => e})

      infix ("filter") ((MString ==> MBoolean, MAny) :: Table) implements composite ${
        val _width = array_range(0, width($self))
        val indices = $self.getColumns($2)
        $self.cleanup($self.filter(_width, indices, $1, contains, or))
      }
      */

      infix ("filter") ((MString ==> MBoolean, MAny) :: Table) implements composite ${
        val _width = array_range(0, width($self))
        val indices = $self.getColumns($2)
        set_data($self, array_filter[ForgeArray[String]](data($self), row =>
          array_reduce[Boolean](array_zipwith[String, Int, Boolean](row, _width, (cell, index) =>
            if(contains(indices, index)) $1(cell)
            else unit(true)
          ), {(x,y) => or(x,y)}, unit(false))
        ))
        $self
      }

      infix ("map_mapper") ((MArray(MInt), MArray(MInt), MString ==> MString, (MArray(MInt), MInt) ==> MBoolean) :: Table) implements map((SArray, SArray), 0, ${ row =>
        /*
        array_zipwith[String, Int, String](row, $1, (cell, index) =>
          if($4($2, index)) $3(cell)
          else cell
        )
        */
        map_body(row, $1, $2, $3, $4)
      })
  
      infix ("map") ((MString ==> MString, MAny) :: Table) implements composite ${
        val _width = array_range(0, width($self)) 
        val indices = $self.getColumns($2)
        $self.cleanup($self.map_mapper(_width, indices, $1, contains))
      }

/*
      infix ("map") ((SArray ==> SArray) :: Table) implements map((SArray, SArray), ${ e => $1(e) })

      infix ("reduce") (((SSArray, SSArray) ==> SSArray) :: SSArray) implements reduce((SSArray,Table), 0, array_empty[ForgeArray[String]](), ${
        (a,b) => $1(a,b)
      })
      infix i"flatMap") ((SArray ==> SArray) :: Table) implements composite ${
        $self.map($1).reduce(array_union[ForgeArray[String]])
      }

      infix ("flatMap_flatMapper")
*/
      infix ("flatMap") ((MString ==> SArray, MAny) :: Table) implements composite ${
        val _width = array_range(0, width($self)) 
        val indices = $self.getColumns($2)
        set_data($self, array_flatmap[ForgeArray[String], ForgeArray[String]](data($self), row => array_zipwith[String, Int, ForgeArray[String]](row, _width, (cell, index) =>
          if(contains(indices, index)) $1(cell)
          else array(cell)
        )))
        $self // perhaps fold into set_data
      }

      // API 

      // CUT
      infix ("cut") ((MInt, MAny) :: Table) implements composite ${
        if($1 < 0) println("Trying to cut on index: " + $1)
        $self.map((cell => {
          if ($1 ge cell.size) cell  
          else cell.substring(0, $1) + cell.substring($1 + 1)
        }), $2)
      }

      infix ("cut") ((MString, MAny) :: Table) implements composite ${
        $self.map(_.replaceFirst($1, ""), $2)
      }

      infix ("cut") (MString :: Table) implements composite ${
        $self.map(_.replaceFirst($1, ""), array_range(0, width($self)))
      }

      infix ("cut") ((MString ==> MString, MAny) :: Table) implements composite ${
        $self.map((cell => {
          val result = $1(cell)
          val index = cell.indexOf(result)
          if(index eq -1) cell
          else cell.substring(0, index) + cell.substring(index+result.size)
        }), $2)
      }

      infix ("cutRight") ((MString, MAny) :: Table) implements composite ${
        $self.map((cell => {
          val index = cell.lastIndexOf($1)
          if(index eq -1) cell
          else cell.substring(0, index) + cell.substring(index + $1.size)
        }), $2)
      }

      infix ("cutAll") ((MString, MAny) :: Table) implements composite ${
        $self.map(_.replaceAllLiterally($1, ""), $2)
      }

      infix ("cutAll") (MString :: Table) implements composite ${
        $self.map(_.replaceAllLiterally($1, ""), array_range(0, width($self)))
      }

      infix ("cutAll") (MInt :: Table) implements composite ${
        $self.map((cell => {
          if ($1 ge cell.size) cell
          else cell.substring(0, $1) + cell.substring($1 + 1)
        }), array_range(0, width($self)))
      }

      // SPLIT

      infix ("split") ((MInt, MAny) :: Table) implements composite ${
        if($1 < 0) println("Trying to split on index: " + $1)
        $self.flatMap((cell => {
          if ($1 ge cell.size) array(cell, "")
          else array(cell.substring(0, $1), cell.substring($1 + 1)) 
        }), $2)
      }

      infix ("split") ((MString, MAny) :: Table) implements composite ${
        $self.flatMap((cell => {
          val index = cell.indexOf($1)
          if (index eq -1) array(cell, "")
          else array(cell.substring(0, index), cell.substring(index+$1.size))
        }), $2)
      }

      infix ("split") ((MString ==> MString, MAny) :: Table) implements composite ${
        $self.flatMap((cell => {
          val result = $1(cell)
          val index = cell.indexOf(result)
          if (index eq -1) array(cell, "")
          else array(cell.substring(0, index), cell.substring(index+result.size))
        }), $2)
      }

      infix ("splitRight") ((MString, MAny) :: Table) implements composite ${
        $self.flatMap((cell => {
          val index = cell.lastIndexOf($1)
          if (index eq -1) array(cell, "")
          else array(cell.substring(0, index), cell.substring(index+$1.size))
        }), $2)
      }

      infix ("splitAll") ((MString, MAny) :: Table) implements composite ${
        $self.flatMap(_.xsplit($1), $2)
      }

      // EXTRACT

      infix ("extract") ((MInt, MAny) :: Table) implements composite ${
        if($1 < 0) println("Trying to extract on index: " + $1)
        $self.flatMap(cell => {
          if($1 ge cell.size ) array(cell, "")
          else array(cell, cell.xcharAt($1))
        }, $2)
      }

      infix ("extract") ((MString, MAny) :: Table) implements composite ${
        $self.flatMap(cell => {
          if(cell.indexOf($1) eq -1) array(cell, "")
          else array(cell, $1)
        }, $2)
      }

      infix ("extract") ((MString ==> MString, MAny) :: Table) implements composite ${
        $self.flatMap(cell => {
          val result = $1(cell)
          if(cell.indexOf(result) eq -1) array(cell, "")
          else array(cell, result)
          array(cell, cell)
        }, $2)
      }

      // EDIT : just wraps map at the moment 
      infix ("edit") ((MString ==> MString, MAny) :: Table) implements composite ${
        $self.map($1, $2)
      }

/*
      // DELETE - unsupported
      infix ("delete") ((MAny) :: Table) implements composite ${
        println("Delete not currently supported")
        $self
      }
*/
/*
      infix ("delete") ((MString, MAny) :: Table) implements composite ${
        $self.delete({x => regexMatch($1, x)}, $2)
      }
*/

      // (wrap to filter) todododododotodo
      infix ("delete") ((MString ==> MBoolean, MAny) :: Table) implements composite ${
        $self.filter($1, $2)
        $self
      }

      // DROP - todo reforge
      infix ("drop") (MInt :: Table) implements composite ${
        //println("Drop not currently supported")
        set_data($self, array_map[ForgeArray[String], ForgeArray[String]](data($self), {x => dropCell(x, $1)}))
        //(data($self), {x => dropCell(x, $1)}))
        $self
      }

      infix ("drop") (MString :: Table) implements composite ${
        println("Drop not currently supported")
        $self
      }
    
    /*
      infix ("drop") (Seq[Any] :: Table) implements composite ${
        println("Drop not currently supported")
        $self
      }
    */
    
      // MERGE

      // TRANSPOSE

      // TRANSLATE

      // FOLD

      // UNFOLD

      // WRAP
    
/*
      infix ("wrapShort") (MInt :: Table) implements composite ${
        set_data($self, 
          table.map{col => col.grouped(wrap).toArray}.map(x=>x.transpose).flatMap(x => x)
        )
        $self
      }
      def wrapLong(wrap: Int) = {
        val grouper = sc.parallelize(for (i <- 0 until table.count().toInt) yield i / wrap).coalesce(table.partitions.size)
        copy(table.zip(grouper).groupBy(_._2).map(_._2.map(_._1)).flatMap(x=>x))
      }

      infix ("wrap") (MInt, :: Table) implements composite ${
        if($1 < width($self)) $self.wrapShort($1)
        else $self.wrapLong($1)
      }
*/

      // IO - could be better
      infix ("tableFromFile") (MString :: Table) implements composite ${ 
        val d = fromFile($1)
        Table(d, array_length(d), map_empty[String, Int](), parseFileName($1))
      }

      infix ("tableToFile") (MString :: MUnit, effect = simple) implements composite ${
        toFile($1, name($self), data($self), header($self))
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
 
