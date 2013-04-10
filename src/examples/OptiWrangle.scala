package ppl.dsl.forge
package examples

import core.{ForgeApplication, ForgeApplicationRunner}

object OptiWrangleRunner extends ForgeApplicationRunner with OptiWrangle

trait OptiWrangle extends ForgeApplication with ScalaOps {

  //name of the DSL
  def dslName = "OptiWrangle"

  //DSL specification
  def specification() = {
    // Include Scala Ops
    addScalaOps()

    //Types
    val DataWrangler = tpe("DataWrangler")
    val AS = tpeInst(GArray(tpePar("T")), List(MString))
    val AAS = tpeInst(GArray(tpePar("T")), List(AS))

    // note the table is column major
    data(DataWrangler, List(), ("table", AAS), ("header", AS))    

    val stream = ForgePrinter()

    /**
    *   Not completely clear if I should be doing this - btu creating shortcut methods to table
    */
    val dw_new = op (DataWrangler) ("apply", static, List(), List(AAS, AS), DataWrangler, codegenerated, effect=mutable)
    codegen (dw_new) ($cala, "new " + dw_new.tpeName + "("+quotedArg(0)+","+quotedArg(1)+")")
  
    // Data Manipulation Operations
    val dw_promote = op (DataWrangler) ("promote", infix, List(), List(DataWrangler, ("row", MInt, "0")), MUnit, codegenerated, effect=write(0)) //effect?
 
    codegen (dw_promote) ($cala, stream.printLines(
      "if ("+quotedArg("row")+" < "+quotedArg(0)+".table(0).length) {",
      quotedArg(0) + ".header = "+quotedArg(0)+".table.map(col => col("+quotedArg("row")+")).toArray",
      quotedArg(0) +".table = " + quotedArg(0)+".table.map(col => col.take("+quotedArg("row")+") ++ col.drop("+quotedArg("row")+"+1))",
      "}"
    ))

    // I/O operations

    // todo - autodetect headers or lack-there-of todo change to DataWrangler
    val dw_fromFile = op (DataWrangler) ("apply", static, List(), List(MString, MString), DataWrangler, codegenerated)    
    //val dw_fromFile = op (DataWrangler) ("apply", static, List(), List(MString, MString, MString), DataWrangler, codegenerated)    
    codegen (dw_fromFile) ($cala, stream.printLines(
      //"import io.Source",
      "val lines = io.Source.fromFile("+quotedArg(0)+").getLines().map(x => x.split("+quotedArg(1)+").toArray).toArray",
      // todo conditionally lines.drop(1)
      "new " +dw_fromFile.tpeName+"(lines.transpose, null)"  
    ))

    val dw_writeToFile = op (DataWrangler) ("write", infix, List(), List(DataWrangler, MString, MString), MUnit, codegenerated, effect = simple)
    codegen (dw_writeToFile) ($cala, stream.printLines(
      "val ps = new java.io.PrintStream(new java.io.FileOutputStream("+quotedArg(1)+"))",
      "if("+quotedArg(0)+".header != null) ps.println("+quotedArg(0)+".header.mkString(\",\"))",
      quotedArg(0) + ".table.transpose foreach {row => ps.println(row.mkString(\",\"))}",
      "ps.close()"
    ))

    ()
  }
}
