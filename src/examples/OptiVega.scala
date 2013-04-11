package ppl.dsl.forge
package examples

import core.{ForgeApplication,ForgeApplicationRunner}

object OptiVegaRunner extends ForgeApplicationRunner with OptiVega

trait OptiVega extends ForgeApplication with ScalaOps {
  /**
   * The name of your DSL. This is the name that will be used in generated files,
   * package declarations, etc.
   */
  def dslName = "OptiVega"
    
  def addOptiVega() {
    ov_specification()
  }

  def specification() = {
    ov_specification()
  }

  /**
   * The specification is the DSL definition (types, data structures, ops, code generators)
   */
  def ov_specification() = {
    /**
     * Include Scala ops
     */
     addScalaOps()
        
    /**
     * Types
     */
    val Vega = tpe("Vega")

    /**
     * Data structures
     */
    data(Vega, List())
    
    /* Generic formatting instance */
    val stream = ForgePrinter()
        
    /**
     * Ops
     */           
        
    val vega_new = op (Vega) ("apply", static, List(), List(), Vega, codegenerated)
    codegen (vega_new) ($cala, "new "+vega_new.tpeName+"()")
    
    val boxString = stringLiteral("{\"label\": \"R_LABEL\", \"mean\": 1, \"lo\" : 0, \"hi\": 2}")
    //val boxString = "\"{\\\"label\\\": \\\"R_LABEL\\\", \\\"mean\\\": 1, \\\"lo\\\": 0, \\\"hi\\\": 2},\""
    val vega_plot = op (Vega) ("plot", infix, List(), List(Vega), MString, codegenerated)
    codegen (vega_plot) ($cala, stream.printLines(
    "val baseString = "+boxString,
    "val dataString = baseString.replaceAll(\"R_LABEL\", \"HelloWorld\")",
    "scala.io.Source.fromFile(\"/afs/cs.stanford.edu/u/gibbons4/data/VegaBoxPlot.json\").mkString",
    ".replaceAll(\"REPLACE_ME\", dataString)"
    ))
    
    ()
  }
}
 
