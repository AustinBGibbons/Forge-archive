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
    val BoxPlot = tpe("BoxPlot")

    /**
     * Data structures
     */
    data(Vega, List())
    data(BoxPlot, List(), ("label", MString), ("mean", MAny), ("lo", MAny), ("hi", MAny))    

    /* Generic formatting instance */
    val stream = ForgePrinter()
        
    /**
     * Ops
     */           
        
    val vega_new = op (Vega) ("apply", static, List(), List(), Vega, codegenerated)
    codegen (vega_new) ($cala, "new "+vega_new.tpeName+"()")
    val bp_new = op (BoxPlot) ("apply", static, List(), List(MString, MAny, MAny, MAny), BoxPlot, codegenerated)
    codegen (bp_new) ($cala, "new "+bp_new.tpeName+"("+(0 to 3).map(quotedArg).mkString(",")+")")

    val boxString = stringLiteral("{\"label\": \"R_LABEL\", \"mean\": R_MEAN, \"lo\" : R_LO, \"hi\": R_HI}")
    val vega_plot = op (Vega) ("plot", infix, List(), List(Vega, ("plot", MString), ("data", tpeInst(GArray(tpePar("T")), List(BoxPlot)))), MString, codegenerated)
    codegen (vega_plot) ($cala, stream.printLines(
      //"val baseString = plotStrings.getOrElse("+quotedArg("plot")+"), \"BAD_PLOT_TYPE\")", // TODO BAD_PLOT_TYPE
      "val baseString = " + boxString, // TODO BAD_PLOT_TYPE
      //"val dataString = "+quotedArg("data")+".map{p => replaceFunctions.get("+quotedArg("plot")+").get.apply(baseString, p)}",
      "val dataString = "+quotedArg("data")+".map{p => baseString.replaceAll(\"R_LABEL\", p.label.toString).replaceAll(\"R_MEAN\", p.mean.toString).replaceAll(\"R_LO\", p.lo.toString).replaceAll(\"R_HI\", p.hi.toString)}",
      "val template = \"Box.json\"",
      "scala.io.Source.fromFile(\"/afs/cs.stanford.edu/u/gibbons4/data/VegaTemplates/\" + template).mkString",
      ".replaceAll(\"REPLACE_ME\", dataString.mkString(\"\\n\"))"
    ))

      //"val dataString = "+quotedArg("data")+".map{p => baseString.replaceAll(\"R_LABEL\", p.label.toString).replaceAll(\"R_MEAN\", p.mean.toString).replaceAll(\"R_LO\", p.lo.toString).replaceAll(\"R_HI\", p.hi.toString)}",
    /*
    val replaceFunctions = Map(

    )

    // should this also be a function
    val plotStrings = Map(
      "arc" -> stringLiteral("R_DATA"),
      "area" -> stringLiteral("{\"x\": R_X,  \"y\": R_Y}"),
      "bar" -> stringLiteral("{\"x\": R_X,  \"y\": R_Y}"),
      "barley" -> stringLiteral(""),
      "choropleth" -> stringLiteral(""),
      "box" -> stringLiteral("{\"label\": \"R_LABEL\", \"mean\": R_MEAN, \"lo\" : R_LO, \"hi\": R_HI}"), // alternatively known as error
      "force" -> stringLiteral(""),
      "image" -> stringLiteral("{\"x\": R_X,  \"y\": R_Y, \"img\": \"R_IMG\"}"),
      "lifelines" -> stringLiteral("{\"label\":R_LABEL, \"born\":R_BORN, \"died\":R_DIED,\"enter\":R_ENTER, \"leave\": R_LEAVE}"),
      "map" -> stringLiteral(""),
      "population" -> stringLiteral(""),
      "scatter" -> stringLiteral(""),
      "stacked_area" -> stringLiteral("{\"x\": R_X,  \"y\": R_Y}"),
      "stacked_bar" -> stringLiteral("{\"x\": R_X,  \"y\": R_Y}"),
      "stocks" -> stringLiteral(""),
      "treemap" -> stringLiteral("R_DATA"),
      "wordmap" -> stringLiteral("{\"text\": R_TEXT, \"value\": R_VALUE},")
    )
    */
    ()
  }
}
 
