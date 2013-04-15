package ppl.dsl.forge
package examples

import core.{ForgeApplication,ForgeApplicationRunner}

object ProfileDSLRunner extends ForgeApplicationRunner with ProfileDSL

trait ProfileDSL extends ForgeApplication with ScalaOps with OptiVega {
  /**
   * The name of your DSL. This is the name that will be used in generated files,
   * package declarations, etc.
   */
  override def dslName = "Profile"
    
  /**
   * The specification is the DSL definition (types, data structures, ops, code generators)
   */
  override def specification() = {
    /**
     * Include Scala ops
     */
    //addScalaOps()  
    addOptiVega()
        
    /**
     * Types
     */
    val Profile = tpe("Profile") 
    
    /**
     * Data structures
     */
    val AD = tpeInst(GArray(tpePar("T")), List(MDouble))
    data(Profile, List(), ("_length", MInt), ("_data", AD))
    
    /* Generic formatting instance */
    val stream = ForgePrinter()
        
    /**
     * Ops
     * 
     * We could simplify this by reusing templates even more, i.e. specializing for different types
     * (e.g. accept a list of binary zip ops that only differentiate in function applied)
     */           
    val pnew = op (Profile) ("apply", static, List(), List(MInt), Profile, codegenerated, effect = mutable)
    val plength = op (Profile) ("length", infix, List(), List(Profile), MInt, codegenerated)    
    val papply = op (Profile) ("apply", infix, List(), List(Profile,MInt), MDouble, codegenerated)
    val pupdate = op (Profile) ("update", infix, List(), List(Profile,MInt,MDouble), MUnit, codegenerated, effect = write(0))
    
    val pprint = op (Profile) ("pprint", infix, List(), List(Profile), MUnit, foreach((MDouble,Profile), 0, "a => println(a)"), effect = simple) // will print out of order in parallel, but hey
     
/*
    val ptimes = op (Profile) ("times", infix, List(), List(Profile, MThunk(MAny)), Profile, codegenerated)
    codegen (ptimes) ($cala, stream.printLines(
        "val out = "+quotedArg(0),
        "var i = 0",        
        "while (i < out.length) {",
        "  val start = System.currentTimeMillis()",
        blockResult(ptimes, 1),
        "  val end = System.currentTimeMillis()",
        "  val duration = (end - start)/1000d ",
        "  out(i) = duration",
        "  i += 1",
        "}",
        "out"
      ))  
    */       
    /*
    val ptimes: Rep[DSLOp] = op (Profile) ("times", infix, List(), List(Profile, MThunk(MAny)), Profile, map ((MUnit, MDouble, Profile), 0, "() => " + stream.printLines(
        "  val start = System.currentTimeMillis()",
        blockResult(ptimes, 1),
        "  val end = System.currentTimeMillis()",
        "  (end - start)/1000d "
    )))
    */   
    val ptimes: Rep[DSLOp] = op (Profile) ("times", infix, List(), List(Profile, MThunk(MAny)), Profile, map ((MDouble, MDouble, Profile), 0, "x => {" + stream.printLines(
        "  val start = System.currentTimeMillis()",
        quotedArg(1),
        "  val end = System.currentTimeMillis()",
        "  (end - start)/1000d }"
    )))
       
/*
    val pvegaData = op (Profile) ("vegaData", infix, List(), List(Profile), (MString, MDouble, MDouble, MDouble), codegenerated)
    codegen (pvegaData) ($cala, stream.printLines(
    "val mean = "+quotedArg(0)+"._data.reduce(_+_) / "+quotedArg(0) + ".length",
    "val lo = "+quotedArg(0)+"._data.sortWith(_ < _).head",
    "val hi = "+quotedArg(0)+"._data.sortWith(_ > _).head",
    "(\"Profile\", mean, lo, hi)"
    ))
*/
    val pplot = op (Profile) ("plot", infix, List(), List(Profile), MString, single(MString, {
      stream.printLines(
        "val vega = Vega()",
        "vega.plot(graph=\"box\", data=Array(BoxPlot(\"Profile\", 2, 1, 4)))"
        //"Vega().plot(\"box\", data=Array((\"Profile\", mean, lo, hi)))"
    )}))

    /**
     * DeliteCollectionification
     * This enables a tpe to be passed in as the collection type of a Delite op
     */
    Profile is DeliteCollection(MDouble, pnew, plength, papply, pupdate)
    
    /**
     * Code generators
     */
      
    // TODO: how do we refer to other methods or codegenerators inside a particular codegen impl? e.g. vfoo uses vlength   
    codegen (pnew) ($cala, "new "+pnew.tpeName+"("+quotedArg(0)+", new Array[Double]("+quotedArg(0)+"))")
    codegen (plength) ($cala, quotedArg(0) + "._length")
    codegen (papply) ($cala, quotedArg(0) + "._data.apply(" + quotedArg(1) + ")")
    codegen (pupdate) ($cala, quotedArg(0) + "._data.update(" + quotedArg(1) + ", " + quotedArg(2) + ")")    
    ()
  }
}
 
