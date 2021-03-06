package ppl.dsl.forge
package templates
package compiler

import java.io.{BufferedWriter, FileWriter, PrintWriter}
import scala.tools.nsc.io._
import scala.collection.mutable.ArrayBuffer
import scala.virtualization.lms.common._

import core._
import shared.BaseGenOps
import Utilities._

trait DeliteGenOps extends BaseGenOps {  
  this: ForgeCodeGenDelite =>
  
  val IR: ForgeApplicationRunner with ForgeExp with ForgeOpsExp
  import IR._

  def baseOpsCls(opsGrp: DSLOps) = {
    if (opsGrp.ops.exists(_.style == compilerMethod)) opsGrp.grp.name + "CompilerOps" 
    else opsGrp.name    
  }
  def baseExpCls(grp: Rep[DSLGroup]) = {
    // in order of decreasing inclusiveness
    if (grpIsTpe(grp) && ForgeCollections.contains(grpAsTpe(grp)) && DataStructs.contains(grpAsTpe(grp))) "DeliteCollectionOpsExp with DeliteStructsExp"
    else if (grpIsTpe(grp) && ForgeCollections.contains(grpAsTpe(grp))) "DeliteCollectionOpsExp"
    else if (grpIsTpe(grp) && DataStructs.contains(grpAsTpe(grp))) "BaseFatExp with DeliteStructsExp"
    else "BaseFatExp with EffectExp" // we use codegen *GenFat, which requires EffectExp
  }
  
  override def quote(x: Exp[Any]): String = x match {
    case Def(QuoteBlockResult(func,args,ret,captured)) =>
      // bind function args to captured args
      var boundStr: String = ""
      var i = 0
      if (!isThunk(func.tpe)) {
        for (a <- args) {
          // have to be careful about automatic string lifting here
          val add: String = (nl + "\"" + "val " + replaceWildcards(quotedArg(boundArgName(func,a))) + " = " + replaceWildcards(captured(i)) + "\\n\"") 
          boundStr += add
          i += 1
        }
      }            
      // the new-line formatting is admittedly weird; we are using a mixed combination of actual new-lines (for string splitting at Forge)
      // and escaped new-lines (for string splitting at Delite), based on how we received strings from string interpolation.
      // FIX: using inconsistent newline character, not platform independent
      "{ \"" + boundStr + 
       nl + "emitBlock(" + func.name + ")" +
       nl + "quote(getBlockResult(" + func.name + "))+\"\\n\"" + nl + " \" } "
       
    case Def(QuoteSeq(argName)) => "Seq("+unquotes(argName+".map(quote).mkString("+quotes(",")+")")+")"
    
    case Const(s: String) if quoteLiterally => s  // no quotes, wildcards will be replaced later in inline
    case Const(s: String) => replaceWildcards(super.quote(s))  // quote first, then insert wildcards
    
    case _ => super.quote(x)
  }  
  
  // IR node names
  def makeOpNodeName(o: Rep[DSLOp]) = {
    val i = nameClashId(o)    
    o.style match {
      case `staticMethod` => o.grp.name + i + "Object_" + sanitize(o.name).capitalize
      case `compilerMethod` => o.name.capitalize
      case _ => o.grp.name + i + "_" + sanitize(o.name).capitalize
    }
  }  

  // non-thunk functions use bound sym inputs, so we need to add the bound syms to the args  
  // we need both the func name and the func arg name to completely disambiguate the bound symbol, but the unique name gets quite long..
  // could use a local numbering scheme inside each op. should also consider storing the bound syms inside the case class body, instead of the parameter list, which could simplify things.
  def boundArgName(func: Rep[DSLArg], arg: Rep[DSLArg]) = "f_" + func.name  + "_" + simpleArgName(arg)
  def boundArgAnonName(func: Rep[DSLArg], arg: Rep[DSLArg], i: Int) = "f_" + opArgPrefix + i + "_" + simpleArgName(arg)
  def makeArgsWithBoundSyms(args: List[Rep[DSLArg]], opType: OpType) = 
    makeArgs(args, t => t match {
      case Def(Arg(name, f@Def(FTpe(fargs,ret,freq)), d2)) if opType.isInstanceOf[CodeGen] && !isThunk(f) => (simpleArgName(t) :: fargs.map(a => boundArgName(t,a))).mkString(",")
      case _ => simpleArgName(t)
  })
  def makeArgsWithBoundSymsWithType(args: List[Rep[DSLArg]], opType: OpType, typify: Rep[DSLType] => String = repify) = 
    makeArgs(args, t => t match {
      case Def(Arg(name, f@Def(FTpe(fargs,ret,freq)), d2)) if opType.isInstanceOf[CodeGen] && !isThunk(f) => ((simpleArgName(t) + ": " + typify(t.tpe)) :: fargs.map(a => boundArgName(t,a) + ": " + typify(a.tpe))).mkString(",")
      case _ => argify(t, typify)
  })  
      
  def makeOpSimpleNodeNameWithArgs(o: Rep[DSLOp]) = makeOpNodeName(o) + makeArgsWithBoundSyms(o.args, Impls(o))
  def makeOpSimpleNodeNameWithAnonArgs(o: Rep[DSLOp]) = {
    // scalac typer error unless we break up the expression
    val z = o.args.zipWithIndex.map{ case (a,i) => arg(opArgPrefix + i, a.tpe, a.default) }
    makeOpNodeName(o) + makeArgsWithBoundSyms(z, Impls(o))
    // makeOpNodeName(o) + makeArgsWithBoundSyms(o.args.zipWithIndex.map{ case (a,i) => arg(opArgPrefix + i, a.tpe, a.default) })
  }
  
  // TODO: tpeArg should be a List that is the same length as the tpePars in hkTpe
  def makeTpeInst(hkTpe: Rep[DSLType], tpeArg: Rep[DSLType]) = hkTpe match {
    case Def(Tpe(s,Nil,stage)) => s // rather lenient, might get strange results in an improperly specified dsl
    case Def(Tpe(s,List(z),stage)) => s + "[" + quote(tpeArg) + "]"
    case Def(Tpe(s,args,stage)) => err("tried to instantiate tpe " + hkTpe.name + " with arg " + tpeArg.name + ", but " + hkTpe.name + " requires " + args.length + " type parameters")
  }  
  
  def makeTpeClsPar(b: TypeClassSignature, t: Rep[DSLType]) = {
    val body = opIdentifierPrefix + "." + b.prefix + t.name
    b.wrapper match {
      case Some(w) => w + "(" + body + ")"
      case None => body      
    }
  }
        
  def emitImplMethod(o: Rep[DSLOp], func: Rep[String], stream: PrintWriter, indent: Int = 0) {
    emitWithIndent(makeOpImplMethodSignature(o) + " = {", stream, indent)
    inline(o, func, quoteLiteral).split(nl).foreach { line => emitWithIndent(line, stream, indent+2 )}
    emitWithIndent("}", stream, indent)
    stream.println()    
  }

  def emitImpls(opsGrp: DSLOps, stream: PrintWriter) {
    emitBlockComment("SingleTask and Composite Impls", stream)   
    stream.println()
    stream.println("trait " + opsGrp.name + "Impl {")
    stream.println("  this: " + dsl + "Compiler with " + dsl + "Lift => ")
    stream.println()    
    for (o <- unique(opsGrp.ops)) { 
      Impls(o) match {
        case single:SingleTask => emitImplMethod(o,single.func,stream,2)
        case composite:Composite => emitImplMethod(o,composite.func,stream,2)
        case _ => 
      }
    }            
    stream.println("}")
  }
  
  def emitOpExp(opsGrp: DSLOps, stream: PrintWriter) {
    emitBlockComment("IR Definitions", stream)   
    stream.println()
    
    stream.println("trait " + opsGrp.name + "Exp extends " + baseOpsCls(opsGrp) + " with " + baseExpCls(opsGrp.grp) + " {")
    stream.println("  this: " + dsl + "Exp => ")
    stream.println()
       
    val uniqueOps = unique(opsGrp.ops)
    
    emitIRNodes(uniqueOps, stream)
    stream.println()
    emitNodeConstructors(uniqueOps, stream)
    stream.println()
    emitSyms(uniqueOps, stream)
    stream.println()
    emitAliasInfo(uniqueOps, stream)    
    stream.println()
    emitMirrors(uniqueOps, stream)
    stream.println()    
    emitDeliteCollection(opsGrp.grp, stream)   
    stream.println()
    emitStructMethods(opsGrp.grp, stream)   
    stream.println("}")      
  }
      
  def emitIRNodes(uniqueOps: List[Rep[DSLOp]], stream: PrintWriter) {
    def hasIRNode(o: Rep[DSLOp]) = Impls(o) match {
      case _:Composite | _:Getter | _:Setter => false
      case _ => true
    }
    
    def emitOpNodeHeader(o: Rep[DSLOp], opStr: String) {
      stream.println(" extends " + opStr + " {") 
      for (targ <- o.tpePars) {
        for (b <- targ.ctxBounds) {
          stream.println("    val " + b.prefix + targ.name + " = implicitly[" + b.name + "[" + targ.name + "]]")
        }
      }
    }
    
    def emitOpNodeFooter(o: Rep[DSLOp]) {
      stream.println("  }")        
    }
    
    // IR nodes
    for (o <- uniqueOps if hasIRNode(o)) { 
      stream.print("  case class " + makeOpNodeName(o) + makeTpeParsWithBounds(o.tpePars))
      if (Impls(o).isInstanceOf[CodeGen]) stream.print(makeArgsWithBoundSymsWithType(o.args, Impls(o), blockify))
      else stream.print(makeOpArgsWithType(o))    
      stream.print(makeOpImplicitArgsWithType(o,true))
      
      Impls(o) match {
        case codegen:CodeGen =>           
          emitOpNodeHeader(o, "Def[" + quote(o.retTpe) + "]") 
        case single:SingleTask =>
          emitOpNodeHeader(o, "DeliteOpSingleTask[" + quote(o.retTpe) + "](reifyEffectsHere("+makeOpImplMethodNameWithArgs(o)+"))")
        case Allocates(tpe,init) =>
          emitOpNodeHeader(o, "DeliteStruct[" + quote(o.retTpe) + "]")
          val data = DataStructs(tpe)
          val elemsPure = data.fields.zip(init) map { case ((name,t),i) => ("\""+name+"\"", inline(o,i,quoteLiteral)) }
          val elems = if (o.effect == mutable) elemsPure map { case (k,v) => (k, "var_new("+v+").e") } else elemsPure
          stream.println("    val elems = copyTransformedElems(collection.Seq(" + elems.mkString(",") + "))")
        case map:Map =>
          val colTpe = getHkTpe(o.retTpe)
          val dc = ForgeCollections(colTpe)
          emitOpNodeHeader(o, "DeliteOpMap[" + quote(map.tpePars._1) + "," + quote(map.tpePars._2) + "," + makeTpeInst(colTpe, map.tpePars._2) + "]")            
          stream.println()
          stream.println("    val in = " + o.args.apply(map.argIndex).name)
          stream.println("    def func = " + inline(o,map.func,quoteLiteral))
          stream.println("    override def alloc(len: Exp[Int]) = " + makeOpMethodName(dc.alloc) + makeTpePars(instAllocReturnTpe(dc.alloc.tpePars, map.tpePars._2)) + "(in, len)")
          stream.println("    val size = copyTransformedOrElse(_.size)(" + makeOpMethodNameWithArgs(dc.size) + ")")          
        case zip:Zip => 
          val colTpe = getHkTpe(o.retTpe)
          val dc = ForgeCollections(colTpe)
          emitOpNodeHeader(o, "DeliteOpZipWith[" + quote(zip.tpePars._1) + "," + quote(zip.tpePars._2) + "," + quote(zip.tpePars._3) + "," + makeTpeInst(colTpe,zip.tpePars._3) + "]")            
          stream.println()
          stream.println("    val inA = " + o.args.apply(zip.argIndices._1).name)
          stream.println("    val inB = " + o.args.apply(zip.argIndices._2).name)
          stream.println("    def func = " + inline(o,zip.func,quoteLiteral))
          stream.println("    override def alloc(len: Exp[Int]) = " + makeOpMethodName(dc.alloc) + makeTpePars(instAllocReturnTpe(dc.alloc.tpePars, zip.tpePars._3)) + "(inA, len)")
          stream.println("    val size = copyTransformedOrElse(_.size)(" + makeOpMethodNameWithArgs(dc.size) + ")")
        case reduce:Reduce =>
          val col = o.args.apply(reduce.argIndex)
          val dc = ForgeCollections(getHkTpe(col.tpe))
          emitOpNodeHeader(o, "DeliteOpReduce[" + quote(reduce.tpePar) + "]")            
          stream.println()
          stream.println("    val in = " + col.name)
          stream.println("    def func = " + inline(o,reduce.func,quoteLiteral))
          stream.println("    def zero = " + makeOpMethodNameWithArgs(reduce.zero))
          stream.println("    val size = copyTransformedOrElse(_.size)(" + makeOpMethodNameWithArgs(dc.size) + ")")
        case filter:Filter =>
          val colTpe = getHkTpe(o.retTpe)
          val dc = ForgeCollections(colTpe)
          emitOpNodeHeader(o, "DeliteOpFilter[" + quote(filter.tpePars._1) + "," + quote(filter.tpePars._2) + "," + makeTpeInst(colTpe,filter.tpePars._2) + "]")            
          stream.println()
          stream.println("    val in = " + o.args.apply(filter.argIndex).name)
          stream.println("    def cond = " + inline(o,filter.cond,quoteLiteral))
          stream.println("    def func = " + inline(o,filter.func,quoteLiteral))
          stream.println("    override def alloc(len: Exp[Int]) = " + makeOpMethodName(dc.alloc) + makeTpePars(instAllocReturnTpe(dc.alloc.tpePars, filter.tpePars._2)) + "(in, len)")
          stream.println("    val size = copyTransformedOrElse(_.size)(" + makeOpMethodNameWithArgs(dc.size) + ")")                          
        case foreach:Foreach =>
          val col = o.args.apply(foreach.argIndex)
          val dc = ForgeCollections(getHkTpe(col.tpe))
          emitOpNodeHeader(o, "DeliteOpForeach[" + quote(foreach.tpePar) + "]")            
          stream.println()
          stream.println("    val in = " + col.name)
          stream.println("    def func = " + inline(o,foreach.func,quoteLiteral))
          stream.println("    def sync = n => unit(List())")
          stream.println("    val size = copyTransformedOrElse(_.size)(" + makeOpMethodNameWithArgs(dc.size) + ")")                  
      }
      emitOpNodeFooter(o)        
      stream.println()        
    }      
    stream.println()    
  }
  
  def emitNodeConstructors(uniqueOps: List[Rep[DSLOp]], stream: PrintWriter) {
    // methods that construct nodes
    for (o <- uniqueOps) { 
      stream.println("  " + makeOpMethodSignature(o) + " = {")
      val summary = scala.collection.mutable.ArrayBuffer[String]()
      
      if (Impls(o).isInstanceOf[CodeGen]) {
        for (arg <- o.args) {
          arg match {
            case Def(Arg(name, f@Def(FTpe(args,ret,freq)), d2)) =>
              stream.println()
              for (s <- args if s.tpe != byName) {
                emitWithIndent("val " + boundArgName(arg,s) + " = fresh[" + quote(s.tpe) + "]", stream, 4)              
              }              
              val fargs = if (!isThunk(f)) "(" + makeArgs(args, a => boundArgName(arg,a)) + ")" else ""
              emitWithIndent("val b_" + name + " = reifyEffects(" + name + fargs + ")", stream, 4)
              emitWithIndent("val sb_" + name + " = summarizeEffects(b_" + name + ")", stream, 4)
              summary += "sb_"+name
            case _ =>
          }
        }
      }
      
      def summarizeEffects(s: scala.collection.mutable.ArrayBuffer[String]): String = {
        if (s.length == 0) ""
        else {
          val rest = summarizeEffects(s.tail)
          if (rest == "") s.head            
          else s.head + " andThen ((" + rest + " andThen " + s.head + ").star)"             
        }
      }
      
      val hasEffects = summary.length > 0
      
      // composites, getters and setters are currently inlined
      // in the future, to support pattern matching and optimization, we should implement these as abstract IR nodes and use lowering transformers
      Impls(o) match {
        case c:Composite => emitWithIndent(makeOpImplMethodNameWithArgs(o), stream, 4)
        case g@Getter(structArgIndex,field) => 
          val struct = o.args.apply(structArgIndex)
          val fieldTpe = DataStructs(getHkTpe(struct.tpe)).fields.find(t => t._1 == field).get.tpe
          emitWithIndent("field["+quote(fieldTpe)+"]("+inline(o,quotedArg(struct.name),quoteLiteral)+",\""+field+"\")", stream, 4)        
        case s@Setter(structArgIndex,field,value) =>
          val struct = o.args.apply(structArgIndex) 
          val fieldTpe = DataStructs(getHkTpe(struct.tpe)).fields.find(t => t._1 == field).get.tpe
          emitWithIndent("field_update["+quote(fieldTpe)+"]("+inline(o,quotedArg(struct.name),quoteLiteral)+",\""+field+"\","+inline(o,value,quoteLiteral)+")", stream, 4)        
        case _ if hasEffects =>
          // if (o.effect != simple) { err("don't know how to generate non-simple effects with functions") }
          val prologue = if (o.effect == simple) " andAlso Simple()" else ""
          val args = "(" + o.args.flatMap(a => a match {
            case Def(Arg(name, f@Def(FTpe(args,ret,freq)), d2)) => 
              val freshSyms = if (isThunk(f)) Nil else args.map(b => boundArgName(a,b)) 
              ("b_" + name) :: freshSyms
            case Def(Arg(name, _, _)) => List(name)          
          }).mkString(",") + ")"
          emitWithIndent(makeEffectAnnotation(simple,o) + "(" + makeOpNodeName(o) + makeTpePars(o.tpePars) + args + makeOpImplicitArgs(o) + ", " + summarizeEffects(summary) + prologue + ")", stream, 4)
        case _ => 
          emitWithIndent(makeEffectAnnotation(o.effect,o) + "(" + makeOpNodeName(o) + makeTpePars(o.tpePars) + makeOpArgs(o) + makeOpImplicitArgs(o) + ")", stream, 4)        
      }
      
      emitWithIndent("}", stream, 2)
    }    
  }
  
  def emitSyms(uniqueOps: List[Rep[DSLOp]], stream: PrintWriter) {
    if (uniqueOps.exists(o => o.args.exists(t => t match { case Def(Arg(_, Def(FTpe(a,b,freq)), _)) => true; case _ => false}))) {
      emitBlockComment("Syms", stream, indent=2)
      
      var symsBuf      = "override def syms(e: Any): List[Sym[Any]] = e match {" + nl
      var boundSymsBuf = "override def boundSyms(e: Any): List[Sym[Any]] = e match {" + nl
      var symsFreqBuf  = "override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {" + nl 
      
      def makeSym(o: Rep[DSLOp], wrap: String, addFreq: Boolean = false) = {
        val symsArgs = o.args.collect { 
          case t@Def(Arg(name, Def(FTpe(args,ret,freq)), d2)) => (freq,name,args.filterNot(_.tpe == byName).map(a => boundArgName(t,a)))
          case Def(Arg(name,tpe,d2)) => (normal,name,Nil)
        }          
        if (hasFuncArgs(o)) {
          var symsArgsStr = symsArgs.map { case (f,name,bound) => wrap + (if (addFreq) makeFrequencyAnnotation(f) else "") + "(" + name + ")" }.mkString(":::")
          if (wrap == "effectSyms") symsArgsStr += " ::: List(" + symsArgs.flatMap(t => t._3.map(e => e + ".asInstanceOf[Sym[Any]]")).mkString(",") + ")"
          "    case " + makeOpSimpleNodeNameWithArgs(o) + " => " + symsArgsStr + nl
        }
        else ""
      }
            
      for (o <- uniqueOps if Impls(o).isInstanceOf[CodeGen]) { 
        symsBuf += makeSym(o, "syms") 
        boundSymsBuf += makeSym(o, "effectSyms") 
        symsFreqBuf += makeSym(o, "", addFreq = true) 
      }
    
      symsBuf      += "    case _ => super.syms(e)" + nl + "  }"
      boundSymsBuf += "    case _ => super.boundSyms(e)" + nl + "  }"
      symsFreqBuf  += "    case _ => super.symsFreq(e)" + nl + "  }"
      
      for (buf <- List(symsBuf,boundSymsBuf,symsFreqBuf)) emitWithIndent(buf,stream,2)                  
    }
  }
  
  def emitAliasInfo(uniqueOps: List[Rep[DSLOp]], stream: PrintWriter) {
    if (uniqueOps.exists(o => o.aliasHint != nohint)) {          
      emitBlockComment("Aliases / Sharing", stream, indent=2)
      
      var aliasBuf    = "override def aliasSyms(e: Any): List[Sym[Any]] = e match {" + nl
      var containBuf  = "override def containSyms(e: Any): List[Sym[Any]] = e match {" + nl
      var extractBuf  = "override def extractSyms(e: Any): List[Sym[Any]] = e match {" + nl
      var copyBuf     = "override def copySyms(e: Any): List[Sym[Any]] = e match {" + nl
      
      def makeAliasAnnotation(o: Rep[DSLOp], args: List[Int]) = {
        val rhs = if (args == Nil) "Nil" else args.map(i => "syms(" + o.args.apply(i).name + ")").mkString(":::")        // TODO
        "    case " + makeOpSimpleNodeNameWithArgs(o) + " => " + rhs + nl
      }
      
      def makeAllAliasAnnotations(o: Rep[DSLOp], aliasSyms: Option[List[Int]], containSyms: Option[List[Int]], extractSyms: Option[List[Int]], copySyms: Option[List[Int]]) = {
        aliasSyms.foreach   { l => aliasBuf   += makeAliasAnnotation(o,l) }
        containSyms.foreach { l => containBuf += makeAliasAnnotation(o,l) }
        extractSyms.foreach { l => extractBuf += makeAliasAnnotation(o,l) }
        copySyms.foreach    { l => copyBuf    += makeAliasAnnotation(o,l) }                    
      }
      
      for (o <- uniqueOps if o.aliasHint != nohint) {
        o.aliasHint match {
          case AliasCopies(z) =>
            if (o.args.length == z.length) makeAllAliasAnnotations(o, Some(Nil), Some(Nil), Some(Nil), Some(z)) // == aliasesNone
            else makeAllAliasAnnotations(o, None, None, None, Some(z))
            
          case AliasInfo(al,co,ex,cp) => makeAllAliasAnnotations(o,al,co,ex,cp)
        }
      }
              
      aliasBuf   += "    case _ => super.aliasSyms(e)" + nl + "  }"
      containBuf += "    case _ => super.containSyms(e)" + nl + "  }"
      extractBuf += "    case _ => super.extractSyms(e)" + nl + "  }"
      copyBuf    += "    case _ => super.copySyms(e)" + nl + "  }"
      
      for (buf <- List(aliasBuf,containBuf,extractBuf,copyBuf)) emitWithIndent(buf,stream,2)                  
    }
  }
  
  def emitMirrors(uniqueOps: List[Rep[DSLOp]], stream: PrintWriter) {
    emitBlockComment("Mirroring", stream, indent=2)
    stream.println("  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {")
          
    for (o <- uniqueOps) {
      // helpful identifiers
      val xformArgs = "(" + o.args.zipWithIndex.flatMap(t => t._1 match {
        case Def(Arg(name, f@Def(FTpe(args,ret,freq)), d2)) if Impls(o).isInstanceOf[CodeGen] && !isThunk(f) => "f("+opArgPrefix+t._2+")" :: args.map(a => boundArgAnonName(t._1,a,t._2) /*+ ".asInstanceOf[Sym[Any]]"*/)           
        case Def(Arg(name, _, _)) => List("f("+opArgPrefix+t._2+")")
      }).mkString(",") + ")"
      
      val implicits = (o.tpePars.flatMap(t => t.ctxBounds.map(b => makeTpeClsPar(b,t))) ++ o.implicitArgs.map(a => opIdentifierPrefix + "." + a.name))
      val implicitsWithParens = if (implicits.length == 0) "" else implicits.mkString("(",",",")")
      
      Impls(o) match {
        case codegen:CodeGen =>
          stream.print("    case " + opIdentifierPrefix + "@" + makeOpSimpleNodeNameWithAnonArgs(o) + " => ")
          // pure version with no func args uses smart constructor
          if (!hasFuncArgs(o)) {
            stream.print(makeOpMethodName(o) + xformArgs)
            stream.print("(" + implicits.mkString(","))          
            // we may need to supply an explicit Overload parameter for the smart constructor
            // relies on conventions established in implicitArgsWithOverload (e.g., guaranteed to always be the last implicit)
            val id = nameClashId(o)
            if (id != "") stream.print(",implicitly[Overload" + id + "]")  
            stream.println(")")
          }
          else {
            stream.print("reflectPure(" + makeOpNodeName(o) + xformArgs + implicitsWithParens + ")")
            stream.println("(mtype(manifest[A]), pos)")
          }
          
          // effectful version
          stream.print("    case Reflect(" + opIdentifierPrefix + "@" + makeOpSimpleNodeNameWithAnonArgs(o) + ", u, es) => reflectMirrored(Reflect(" + makeOpNodeName(o) + xformArgs + implicitsWithParens)
          stream.print(", mapOver(f,u), f(es)))")
          stream.println("(mtype(manifest[A]))")
        case _:DeliteOpType => 
          // pure delite op version
          stream.print("    case " + opIdentifierPrefix + "@" + makeOpSimpleNodeNameWithAnonArgs(o) + " => ")
          stream.print("reflectPure(new { override val original = Some(f," + opIdentifierPrefix + ") } with " + makeOpNodeName(o) + xformArgs + implicitsWithParens + ")")
          stream.println("(mtype(manifest[A]), pos)")
          // effectful delite op version
          stream.print("    case Reflect(" + opIdentifierPrefix + "@" + makeOpSimpleNodeNameWithAnonArgs(o) + ", u, es) => reflectMirrored(Reflect(new { override val original = Some(f," + opIdentifierPrefix + ") } with " + makeOpNodeName(o) + xformArgs + implicitsWithParens)
          stream.print(", mapOver(f,u), f(es)))")
          stream.println("(mtype(manifest[A]))")
        case _ => // no mirror  
      }        
    }
    stream.println("    case _ => super.mirror(e, f)")
    stream.println("  }).asInstanceOf[Exp[A]]")
  }
  
  def emitDeliteCollection(grp: Rep[DSLGroup], stream: PrintWriter) {
    try {
      val tpe = grpAsTpe(grp)
      if (ForgeCollections.contains(tpe)) {
        emitBlockComment("Delite collection", stream, indent=2)
        val dc = ForgeCollections(tpe)        
        val isTpe = "is"+tpe.name
        def asTpe = "as"+tpe.name
        stream.println("  def " + isTpe + "[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = isSubtype(x.tp.erasure,classOf["+makeTpeInst(tpe, tpePar("A"))+"])")
        stream.println("  def " + asTpe + "[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = x.asInstanceOf[Exp["+makeTpeInst(tpe, tpePar("A"))+"]]")
        stream.println()
        stream.println("  override def dc_size[A:Manifest](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = {")
        stream.println("    if (" + isTpe + "(x)) " + makeOpMethodName(dc.size) + "(" + asTpe + "(x))") 
        stream.println("    else super.dc_size(x)")
        stream.println("  }")
        stream.println()
        stream.println("  override def dc_apply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int])(implicit ctx: SourceContext) = {")
        stream.println("    if (" + isTpe + "(x)) " + makeOpMethodName(dc.apply) + "(" + asTpe + "(x), n).asInstanceOf[Exp[A]]")
        stream.println("    else super.dc_apply(x,n)")
        stream.println("  }")
        stream.println()
        stream.println("  override def dc_update[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int], y: Exp[A])(implicit ctx: SourceContext) = {")
        val a = if (dc.tpeArg.tp.runtimeClass == classOf[TypePar]) "A" else quote(dc.tpeArg) // hack!
        stream.println("    if (" + isTpe + "(x)) " + makeOpMethodName(dc.update) + "(" + asTpe + "(x), n, y.asInstanceOf[Exp["+a+"]])") 
        stream.println("    else super.dc_update(x,n,y)")
        stream.println("  }")
        
        if (dc.isInstanceOf[ParallelCollectionBuffer]) {
          val dcb = dc.asInstanceOf[ParallelCollectionBuffer]
          stream.println()          
          stream.println("  override def dc_parallelization[A:Manifest](x: Exp[DeliteCollection[A]], hasConditions: Boolean)(implicit ctx: SourceContext) = {")
          // stream.println("    if (" + isTpe + "(x)) " + makeOpMethodName(dcb.parallelization) + "(" + asTpe + "(x), hasConditions)") 
          stream.println("    if (" + isTpe + "(x)) { if (hasConditions) ParSimpleBuffer else ParFlat } // TODO: always generating this right now") 
          stream.println("    else super.dc_parallelization(x, hasConditions)")
          stream.println("  }")          
          stream.println()
          stream.println("  override def dc_set_logical_size[A:Manifest](x: Exp[DeliteCollection[A]], y: Exp[Int])(implicit ctx: SourceContext) = {")
          stream.println("    if (" + isTpe + "(x)) " + makeOpMethodName(dcb.setSize) + "(" + asTpe + "(x), y)")
          stream.println("    else super.dc_set_logical_size(x,y)")
          stream.println("  }")
          stream.println()
          stream.println("  override def dc_appendable[A:Manifest](x: Exp[DeliteCollection[A]], i: Exp[Int], y: Exp[A])(implicit ctx: SourceContext) = {")
          stream.println("    if (" + isTpe + "(x)) " + makeOpMethodName(dcb.appendable) + "(" + asTpe + "(x), i, y)")
          stream.println("    else super.dc_appendable(x,i,y)")
          stream.println("  }")          
          stream.println()
          stream.println("  override def dc_append[A:Manifest](x: Exp[DeliteCollection[A]], i: Exp[Int], y: Exp[A])(implicit ctx: SourceContext) = {")
          stream.println("    if (" + isTpe + "(x)) " + makeOpMethodName(dcb.append) + "(" + asTpe + "(x), i, y)")
          stream.println("    else super.dc_append(x,i,y)")
          stream.println("  }")          
          stream.println()
          stream.println("  override def dc_alloc[A:Manifest,CA<:DeliteCollection[A]:Manifest](x: Exp[CA], size: Exp[Int])(implicit ctx: SourceContext) = {")
          // HACK: dc_alloc in Delite only has 1 tpe parameter, but dc_alloc in the DSLs can have arbitrarily many (and 1 is in fact insufficient). what should the semantics here be?
          // should we require that dc_alloc in Forge has 2 or less type parameters? i.e., at most [Input,Output]? what else could it need? higher-kinded things won't work...
          stream.println("    if (" + isTpe + "(x)) " + makeOpMethodName(dcb.alloc) + makeTpePars(dcb.alloc.tpePars.map(t => tpePar("A"))) + "(" + asTpe + "(x), size).asInstanceOf[Exp[CA]]")
          stream.println("    else super.dc_alloc[A,CA](x,size)")
          stream.println("  }")          
          stream.println()
          stream.println("  override def dc_copy[A:Manifest](src: Exp[DeliteCollection[A]], srcPos: Exp[Int], dst: Exp[DeliteCollection[A]], dstPos: Exp[Int], size: Exp[Int])(implicit ctx: SourceContext) = {")
          stream.println("    if (" + isTpe + "(src) && " + isTpe + "(dst)) " + makeOpMethodName(dcb.copy) + "(" + asTpe + "(src), srcPos, " + asTpe + "(dst), dstPos, size)")
          stream.println("    else super.dc_copy(src,srcPos,dst,dstPos,size)")
          stream.println("  }")                    
        }
      }
    }
    catch { case _ : MatchError => }
  }
  
  def emitStructMethods(grp: Rep[DSLGroup], stream: PrintWriter) {
    def wrapManifest(t: Rep[DSLType]) = getHkTpe(t) match {
      case Def(Tpe("ForgeArray",args,stage)) => "darrayManifest(m.typeArguments(0))"
      case _ => "manifest[" + quote(t) + "]"
    }
    
    try {
      val tpe = grpAsTpe(grp)
      if (DataStructs.contains(tpe)) {
        val d = DataStructs(tpe)
        val fields = d.fields.zipWithIndex.map { case ((fieldName,fieldType),i) => ("\""+fieldName+"\"", if (isTpePar(fieldType)) "m.typeArguments("+i+")" else wrapManifest(fieldType)) }
        val erasureCls = tpe.name + (if (!tpe.tpePars.isEmpty) "[" + tpe.tpePars.map(t => "_").mkString(",") + "]" else "") 
        
        emitBlockComment("Delite struct", stream, indent=2)
        stream.println("  override def unapplyStructType[T:Manifest]: Option[(StructTag[T], List[(String,Manifest[_])])] = {")
        stream.println("    val m = manifest[T]")        
        stream.println("    if (m.erasure == classOf["+erasureCls+"]) Some((classTag(m), collection.immutable.List("+fields.mkString(",")+")))")  
        stream.println("    else super.unapplyStructType(m)")
        stream.println("  }")
      }
    }
    catch { case _ : MatchError => }
  }
    
  def emitOpCodegen(opsGrp: DSLOps, stream: PrintWriter) {        
    val rules = unique(opsGrp.ops).map(o => (o,Impls(o))).filter(_._2.isInstanceOf[CodeGen])
    if (rules.length > 0){
      emitBlockComment("Code generators", stream)   
      stream.println()
      for (g <- generators) { 
        val generatorRules = rules.flatMap{case (o,i) => i.asInstanceOf[CodeGen].decls.collect{case (k,r) if (k == g) => (o,r)}}
        if (generatorRules.length > 0) {
          stream.println("trait " + g.name + "Gen" + opsGrp.name + " extends " + g.name + "GenFat {")
          stream.println("  val IR: " + opsGrp.name + "Exp")
          stream.println("  import IR._")
          stream.println()
          stream.println("  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {")
          for ((op,r) <- generatorRules) {
            stream.println("    case " + opIdentifierPrefix + "@" + makeOpSimpleNodeNameWithArgs(op) + " => ")
            val body = quote(r.decl).trim.split(nl).toList
            // how do we decide whether to add stream.println?
            def addPrint(s: String) = {
              // hack! (or heuristic, if the glass is half full)
              !s.startsWith("emit")
            }      
            // use stream.print, since the new lines from the original interpolated code block are still there
            val body2 = body map { l => if (addPrint(l)) "stream.print("+l+")" else l }      
            val result = ("stream.println(\"val \"+quote(sym)+\" = {\")" :: body2) :+ "stream.println(\"}\")"
            result.foreach { line => emitWithIndent(line, stream, 6) }   
            stream.println()              
          }
          stream.println("    case _ => super.emitNode(sym, rhs)")
          stream.println("  }")
          stream.println("}")
        }          
      }
    }      
  }  
}
