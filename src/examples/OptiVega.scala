package ppl.dsl.forge
package examples

import core.{ForgeApplication,ForgeApplicationRunner}

object OptiVegaDSLRunner extends ForgeApplicationRunner with OptiVegaDSL

trait OptiVegaDSL extends ForgeApplication with ScalaOps {
  /**
   * The name of your DSL. This is the name that will be used in generated files,
   * package declarations, etc.
   */
  def dslName = "OptiVega"
    
  /**
   * The specification is the DSL definition (types, data structures, ops, code generators)
   */
  def specification() = {
    /**
     * Include Scala ops
     */
     addScalaOps()
        
    /**
     * Types
     */
    val AD = tpeInst(GArray(tpePar("T")), List(MDouble))
    val OptiVega = tpe("OptiVega") 
    val Padding = tpe("Padding")
    val Data = tpe("Data")    
    val Range = tpe("Range")    
    val Domain = tpe("Domain")    
    val Transform = tpe("Transform")
    val Scales = tpe("Scales")    
    val Axes = tpe("Axes")    
    val Marks = tpe("Marks")    
    val MarkProperties = tpe("MarkProperties")
    val MarkProperty = tpe("MarkProperty")
    val MarkData = tpe("MarkData")

    /**
     * Data structures
     */
    data(OptiVega, List(), ("name", MString),
      ("width", MInt),
      ("height", MInt),
      ("padding", Padding), // bottom top left right
      ("data", Data),
      ("scales", Scales),
      ("axes", Axes),
      ("marks", Marks)
    )
    data(Padding, List(), ("bottom", MInt),
      ("top", MInt),
      ("left", MInt),
      ("right", MInt)
    )
    data(Data, List(), ("name", MString),
      ("values", AD),
      ("url", MString),
      ("transform", Transform) // not completely sure what Transform encompasses
    )
    data(Transform, List(), ("type", MString),
      ("value", MString),
      ("by", MString)
    )
    data(Scales, List(), ("name", MString),
      ("type", MString),
      ("nice", MBoolean),
      ("range", Range),
      ("domain", Domain) 
    )
    data(Range, List(), ("low", MInt), ("high", MInt))
    data(Domain, List(), ("data", MString), ("field", MString))
    data(Axes, List(), ("type", MString),
      ("scale", MString),
      ("ticks", MInt)
    )
    data(Marks, List(), ("type", MString),
      ("from", MString), // data -> table?
      ("properties", MarkProperties)
    )
    data(MarkProperties, List(), ("enter", MarkProperty),
      ("update", MarkProperty),
      ("hover", MarkProperty)
    )
    data(MarkProperty, List(), ("x", MarkData),
      ("y", MarkData),
      ("width", MarkData),
      ("height", MarkData),
      ("x2", MarkData),
      ("y2", MarkData),
      ("fill", MarkData),
      ("stroke", MarkData),
      ("startAngle", MarkData),
      ("endAngle", MarkData),
      ("innerRadius", MarkData),
      ("outerRadius", MarkData)
    )
    data(MarkData, List(), ("scale", MString),
      ("scale", MString),
      ("field", MString),
      ("band", MBoolean),
      ("offset", MInt),
      ("value", MInt)
    )
    
    /* Generic formatting instance */
    val stream = ForgePrinter()
        
    /**
     * Ops
     * 
     * We could simplify this by reusing templates even more, i.e. specializing for different types
     * (e.g. accept a list of binary zip ops that only differentiate in function applied)
     */           
    val ov_new = op (OptiVega) ("apply", static, List(), List(MInt), OptiVega, codegenerated, effect = mutable)
    val ov_plot = op (OptiVega) ("vegaPlot", infix, List(), List(OptiVega), MString, codegenerated)
    codegen (pplot) ($cala, stream.printLines(
    ""
    ))

    /**
     * Code generators
     */
      
    codegen (ov_new) ($cala, "new "+ov_new.tpeName+"("+quotedArg(0)+", new Array[Double]("+quotedArg(0)+"))")
    ()
  }
}
 
