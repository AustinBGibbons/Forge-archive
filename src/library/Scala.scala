package ppl.dsl.forge
package library

import core.{ForgeApplication,ForgeApplicationRunner}
import templates.Utilities.nl

/**
 * This file re-implements LMS common ops in Forge.
 * 
 * Note that Delite pulls in some LMS common ops directly, requiring name disambiguation here.
 * However, so far it does not look like there are any major problems (e.g. ambiguous implicits) from including both versions in a DSL.
 */
trait ScalaOps {
  this: ForgeApplication =>
  
  def importScalaOps() = {
    importProxies()
    importPrimitives()
    importMisc()
    importCasts()
    importNumerics()
    importOrdering()
    importStrings()
    importMath()
    importTuples()
  }
    
  /**
   * TODO: Use reflection to auto-generate the ops and code generators
   * Question: how do we handle effects?
   *   withWrites("update", "insert", ...)?
   */
  def importProxies() = {
    // proxy(scala.collection.immutable.Array)      
  }
  
  def importPrimitives() = {
    val Prim = grp("Primitive2") // conflicts with PrimitiveOps from LMS brought in by Delite
    val T = tpePar("T")
    
    lift (Prim) (MBoolean)    
    
    // why do these conflict with Delite Boolean ops and not the others in Prim?
    // the reason seems to be because of the combination of overloaded parameters that already exist in the LMS versions of the other ops. (i.e., we got lucky)
    direct (Prim) ( "not", Nil, MBoolean :: MBoolean) implements codegen ($cala, ${!$0})
    infix (Prim) ("unary_!", Nil, MBoolean :: MBoolean) implements codegen($cala, "!" + quotedArg(0))
    infix (Prim) ("||", Nil, (MBoolean, MBoolean) :: MBoolean) implements codegen($cala, quotedArg(0) + " || " + quotedArg(1))
    infix (Prim) ("&&", Nil, (MBoolean, MBoolean) :: MBoolean) implements codegen($cala, quotedArg(0) + " && " + quotedArg(1))
    
    lift (Prim) (MInt)
    lift (Prim) (MFloat)
    lift (Prim) (MDouble)
    
    infix (Prim) ("toInt", T withBound TNumeric, T :: MInt) implements codegen($cala, ${ $0.toInt })
    infix (Prim) ("toFloat", T withBound TNumeric, T :: MFloat) implements codegen($cala, ${ $0.toFloat })
    infix (Prim) ("toDouble", T withBound TNumeric, T :: MDouble) implements codegen($cala, ${ $0.toDouble })
    
    fimplicit (Prim) ("repInt2ToRepDouble", Nil, MInt :: MDouble) implements composite ${ $0.toDouble }
    fimplicit (Prim) ("repInt2ToRepFloat", Nil, MInt :: MFloat) implements composite ${ $0.toFloat }
    fimplicit (Prim) ("repFloat2ToRepDouble", Nil, MFloat :: MDouble) implements composite ${ $0.toDouble }
    
    infix (Prim) ("%", Nil, (MInt,MInt) :: MInt) implements codegen($cala, ${$0 % $1})
    
    // specialized versions for primitives     
    // the forge_ prefix is to avoid conflicting with LMS primitive ops
    direct (Prim) ("forge_int_plus", Nil, (MInt,MInt) :: MInt) implements codegen($cala, ${$0 + $1})
    direct (Prim) ("forge_int_minus", Nil, (MInt,MInt) :: MInt) implements codegen($cala, ${$0 - $1})
    direct (Prim) ("forge_int_times", Nil, (MInt,MInt) :: MInt) implements codegen($cala, ${$0 * $1})
    direct (Prim) ("forge_int_divide", Nil, (MInt,MInt) :: MInt) implements codegen($cala, ${$0 / $1})
    
    direct (Prim) ("forge_float_plus", Nil, (MFloat,MFloat) :: MFloat) implements codegen($cala, ${$0 + $1})
    direct (Prim) ("forge_float_minus", Nil, (MFloat,MFloat) :: MFloat) implements codegen($cala, ${$0 - $1})
    direct (Prim) ("forge_float_times", Nil, (MFloat,MFloat) :: MFloat) implements codegen($cala, ${$0 * $1})
    direct (Prim) ("forge_float_divide", Nil, (MFloat,MFloat) :: MFloat) implements codegen($cala, ${$0 / $1})    
    
    direct (Prim) ("forge_double_plus", Nil, (MDouble,MDouble) :: MDouble) implements codegen($cala, ${$0 + $1})
    direct (Prim) ("forge_double_minus", Nil, (MDouble,MDouble) :: MDouble) implements codegen($cala, ${$0 - $1})
    direct (Prim) ("forge_double_times", Nil, (MDouble,MDouble) :: MDouble) implements codegen($cala, ${$0 * $1})
    direct (Prim) ("forge_double_divide", Nil, (MDouble,MDouble) :: MDouble) implements codegen($cala, ${$0 / $1})        
    
    // can we auto-generate these? the tricky part is the bodies, which require explicit conversions..
    // infix (Prim) ("+", Nil, enumerate(CInt,MInt,CFloat,MFloat,CDouble,MDouble)) implements codegen($cala, quotedArg(0) + " + " + quotedArg(1))    
    infix (Prim) ("+", Nil, (CInt,MInt) :: MInt) implements composite ${ forge_int_plus($0,$1) }
    infix (Prim) ("+", Nil, (CInt,MFloat) :: MFloat) implements composite ${ forge_float_plus($0.toFloat,$1) }
    infix (Prim) ("+", Nil, (CInt,MDouble) :: MDouble) implements composite ${ forge_double_plus($0.toDouble,$1) }
    infix (Prim) ("+", Nil, (CFloat,MInt) :: MFloat) implements composite ${ forge_float_plus($0,$1.toFloat) }
    infix (Prim) ("+", Nil, (CFloat,MFloat) :: MFloat) implements composite ${ forge_float_plus($0,$1) }
    infix (Prim) ("+", Nil, (CFloat,MDouble) :: MDouble) implements composite ${ forge_double_plus($0.toDouble,$1) }
    infix (Prim) ("+", Nil, (CDouble,MInt) :: MDouble) implements composite ${ forge_double_plus($0,$1.toDouble) }
    infix (Prim) ("+", Nil, (CDouble,MFloat) :: MDouble) implements composite ${ forge_double_plus($0,$1.toDouble) }
    infix (Prim) ("+", Nil, (CDouble,MDouble) :: MDouble) implements composite ${ forge_double_plus($0,$1) }
    infix (Prim) ("+", Nil, (MInt,CInt) :: MInt) implements composite ${ forge_int_plus($0,$1) }
    infix (Prim) ("+", Nil, (MInt,CFloat) :: MFloat) implements composite ${ forge_float_plus($0.toFloat,$1) }
    infix (Prim) ("+", Nil, (MInt,CDouble) :: MDouble) implements composite ${ forge_double_plus($0.toDouble,$1) }
    infix (Prim) ("+", Nil, (MFloat,CInt) :: MFloat) implements composite ${ forge_float_plus($0,$1.toFloat) }
    infix (Prim) ("+", Nil, (MFloat,CFloat) :: MFloat) implements composite ${ forge_float_plus($0,$1) }
    infix (Prim) ("+", Nil, (MFloat,CDouble) :: MDouble) implements composite ${ forge_double_plus($0.toDouble,$1) }
    infix (Prim) ("+", Nil, (MDouble,CInt) :: MDouble) implements composite ${ forge_double_plus($0,$1.toDouble) }
    infix (Prim) ("+", Nil, (MDouble,CFloat) :: MDouble) implements composite ${ forge_double_plus($0.toDouble,$1) }
    infix (Prim) ("+", Nil, (MDouble,CDouble) :: MDouble) implements composite ${ forge_double_plus($0,$1) }
    infix (Prim) ("+", Nil, (MInt,MInt) :: MInt) implements composite ${ forge_int_plus($0,$1) }
    infix (Prim) ("+", Nil, (MInt,MFloat) :: MFloat) implements composite ${ forge_float_plus($0.toFloat,$1) }
    infix (Prim) ("+", Nil, (MInt,MDouble) :: MDouble) implements composite ${ forge_double_plus($0.toDouble,$1) }
    infix (Prim) ("+", Nil, (MFloat,MInt) :: MFloat) implements composite ${ forge_float_plus($0,$1.toFloat) }
    infix (Prim) ("+", Nil, (MFloat,MFloat) :: MFloat) implements composite ${ forge_float_plus($0,$1) }
    infix (Prim) ("+", Nil, (MFloat,MDouble) :: MDouble) implements composite ${ forge_double_plus($0.toDouble,$1) }
    infix (Prim) ("+", Nil, (MDouble,MInt) :: MDouble) implements composite ${ forge_double_plus($0,$1.toDouble) }
    infix (Prim) ("+", Nil, (MDouble,MFloat) :: MDouble) implements composite ${ forge_double_plus($0,$1.toDouble) }
    infix (Prim) ("+", Nil, (MDouble,MDouble) :: MDouble) implements composite ${ forge_double_plus($0,$1) }    
    
    infix (Prim) ("-", Nil, (CInt,MInt) :: MInt) implements composite ${ forge_int_minus($0,$1) }
    infix (Prim) ("-", Nil, (CInt,MFloat) :: MFloat) implements composite ${ forge_float_minus($0.toFloat,$1) }
    infix (Prim) ("-", Nil, (CInt,MDouble) :: MDouble) implements composite ${ forge_double_minus($0.toDouble,$1) }
    infix (Prim) ("-", Nil, (CFloat,MInt) :: MFloat) implements composite ${ forge_float_minus($0,$1.toFloat) }
    infix (Prim) ("-", Nil, (CFloat,MFloat) :: MFloat) implements composite ${ forge_float_minus($0,$1) }
    infix (Prim) ("-", Nil, (CFloat,MDouble) :: MDouble) implements composite ${ forge_double_minus($0.toDouble,$1) }
    infix (Prim) ("-", Nil, (CDouble,MInt) :: MDouble) implements composite ${ forge_double_minus($0,$1.toDouble) }
    infix (Prim) ("-", Nil, (CDouble,MFloat) :: MDouble) implements composite ${ forge_double_minus($0,$1.toDouble) }
    infix (Prim) ("-", Nil, (CDouble,MDouble) :: MDouble) implements composite ${ forge_double_minus($0,$1) }
    infix (Prim) ("-", Nil, (MInt,CInt) :: MInt) implements composite ${ forge_int_minus($0,$1) }
    infix (Prim) ("-", Nil, (MInt,CFloat) :: MFloat) implements composite ${ forge_float_minus($0.toFloat,$1) }
    infix (Prim) ("-", Nil, (MInt,CDouble) :: MDouble) implements composite ${ forge_double_minus($0.toDouble,$1) }
    infix (Prim) ("-", Nil, (MFloat,CInt) :: MFloat) implements composite ${ forge_float_minus($0,$1.toFloat) }
    infix (Prim) ("-", Nil, (MFloat,CFloat) :: MFloat) implements composite ${ forge_float_minus($0,$1) }
    infix (Prim) ("-", Nil, (MFloat,CDouble) :: MDouble) implements composite ${ forge_double_minus($0.toDouble,$1) }
    infix (Prim) ("-", Nil, (MDouble,CInt) :: MDouble) implements composite ${ forge_double_minus($0,$1.toDouble) }
    infix (Prim) ("-", Nil, (MDouble,CFloat) :: MDouble) implements composite ${ forge_double_minus($0.toDouble,$1) }
    infix (Prim) ("-", Nil, (MDouble,CDouble) :: MDouble) implements composite ${ forge_double_minus($0,$1) }
    infix (Prim) ("-", Nil, (MInt,MInt) :: MInt) implements composite ${ forge_int_minus($0,$1) }
    infix (Prim) ("-", Nil, (MInt,MFloat) :: MFloat) implements composite ${ forge_float_minus($0.toFloat,$1) }
    infix (Prim) ("-", Nil, (MInt,MDouble) :: MDouble) implements composite ${ forge_double_minus($0.toDouble,$1) }
    infix (Prim) ("-", Nil, (MFloat,MInt) :: MFloat) implements composite ${ forge_float_minus($0,$1.toFloat) }
    infix (Prim) ("-", Nil, (MFloat,MFloat) :: MFloat) implements composite ${ forge_float_minus($0,$1) }
    infix (Prim) ("-", Nil, (MFloat,MDouble) :: MDouble) implements composite ${ forge_double_minus($0.toDouble,$1) }
    infix (Prim) ("-", Nil, (MDouble,MInt) :: MDouble) implements composite ${ forge_double_minus($0,$1.toDouble) }
    infix (Prim) ("-", Nil, (MDouble,MFloat) :: MDouble) implements composite ${ forge_double_minus($0,$1.toDouble) }
    infix (Prim) ("-", Nil, (MDouble,MDouble) :: MDouble) implements composite ${ forge_double_minus($0,$1) }    
    
    infix (Prim) ("*", Nil, (CInt,MInt) :: MInt) implements composite ${ forge_int_times($0,$1) }
    infix (Prim) ("*", Nil, (CInt,MFloat) :: MFloat) implements composite ${ forge_float_times($0.toFloat,$1) }
    infix (Prim) ("*", Nil, (CInt,MDouble) :: MDouble) implements composite ${ forge_double_times($0.toDouble,$1) }
    infix (Prim) ("*", Nil, (CFloat,MInt) :: MFloat) implements composite ${ forge_float_times($0,$1.toFloat) }
    infix (Prim) ("*", Nil, (CFloat,MFloat) :: MFloat) implements composite ${ forge_float_times($0,$1) }
    infix (Prim) ("*", Nil, (CFloat,MDouble) :: MDouble) implements composite ${ forge_double_times($0.toDouble,$1) }
    infix (Prim) ("*", Nil, (CDouble,MInt) :: MDouble) implements composite ${ forge_double_times($0,$1.toDouble) }
    infix (Prim) ("*", Nil, (CDouble,MFloat) :: MDouble) implements composite ${ forge_double_times($0,$1.toDouble) }
    infix (Prim) ("*", Nil, (CDouble,MDouble) :: MDouble) implements composite ${ forge_double_times($0,$1) }
    infix (Prim) ("*", Nil, (MInt,CInt) :: MInt) implements composite ${ forge_int_times($0,$1) }
    infix (Prim) ("*", Nil, (MInt,CFloat) :: MFloat) implements composite ${ forge_float_times($0.toFloat,$1) }
    infix (Prim) ("*", Nil, (MInt,CDouble) :: MDouble) implements composite ${ forge_double_times($0.toDouble,$1) }
    infix (Prim) ("*", Nil, (MFloat,CInt) :: MFloat) implements composite ${ forge_float_times($0,$1.toFloat) }
    infix (Prim) ("*", Nil, (MFloat,CFloat) :: MFloat) implements composite ${ forge_float_times($0,$1) }
    infix (Prim) ("*", Nil, (MFloat,CDouble) :: MDouble) implements composite ${ forge_double_times($0.toDouble,$1) }
    infix (Prim) ("*", Nil, (MDouble,CInt) :: MDouble) implements composite ${ forge_double_times($0,$1.toDouble) }
    infix (Prim) ("*", Nil, (MDouble,CFloat) :: MDouble) implements composite ${ forge_double_times($0.toDouble,$1) }
    infix (Prim) ("*", Nil, (MDouble,CDouble) :: MDouble) implements composite ${ forge_double_times($0,$1) }
    infix (Prim) ("*", Nil, (MInt,MInt) :: MInt) implements composite ${ forge_int_times($0,$1) }
    infix (Prim) ("*", Nil, (MInt,MFloat) :: MFloat) implements composite ${ forge_float_times($0.toFloat,$1) }
    infix (Prim) ("*", Nil, (MInt,MDouble) :: MDouble) implements composite ${ forge_double_times($0.toDouble,$1) }
    infix (Prim) ("*", Nil, (MFloat,MInt) :: MFloat) implements composite ${ forge_float_times($0,$1.toFloat) }
    infix (Prim) ("*", Nil, (MFloat,MFloat) :: MFloat) implements composite ${ forge_float_times($0,$1) }
    infix (Prim) ("*", Nil, (MFloat,MDouble) :: MDouble) implements composite ${ forge_double_times($0.toDouble,$1) }
    infix (Prim) ("*", Nil, (MDouble,MInt) :: MDouble) implements composite ${ forge_double_times($0,$1.toDouble) }
    infix (Prim) ("*", Nil, (MDouble,MFloat) :: MDouble) implements composite ${ forge_double_times($0,$1.toDouble) }
    infix (Prim) ("*", Nil, (MDouble,MDouble) :: MDouble) implements composite ${ forge_double_times($0,$1) }    

    infix (Prim) ("/", Nil, (CInt,MInt) :: MInt) implements composite ${ forge_int_divide($0,$1) }
    infix (Prim) ("/", Nil, (CInt,MFloat) :: MFloat) implements composite ${ forge_float_divide($0.toFloat,$1) }
    infix (Prim) ("/", Nil, (CInt,MDouble) :: MDouble) implements composite ${ forge_double_divide($0.toDouble,$1) }
    infix (Prim) ("/", Nil, (CFloat,MInt) :: MFloat) implements composite ${ forge_float_divide($0,$1.toFloat) }
    infix (Prim) ("/", Nil, (CFloat,MFloat) :: MFloat) implements composite ${ forge_float_divide($0,$1) }
    infix (Prim) ("/", Nil, (CFloat,MDouble) :: MDouble) implements composite ${ forge_double_divide($0.toDouble,$1) }
    infix (Prim) ("/", Nil, (CDouble,MInt) :: MDouble) implements composite ${ forge_double_divide($0,$1.toDouble) }
    infix (Prim) ("/", Nil, (CDouble,MFloat) :: MDouble) implements composite ${ forge_double_divide($0,$1.toDouble) }
    infix (Prim) ("/", Nil, (CDouble,MDouble) :: MDouble) implements composite ${ forge_double_divide($0,$1) }
    infix (Prim) ("/", Nil, (MInt,CInt) :: MInt) implements composite ${ forge_int_divide($0,$1) }
    infix (Prim) ("/", Nil, (MInt,CFloat) :: MFloat) implements composite ${ forge_float_divide($0.toFloat,$1) }
    infix (Prim) ("/", Nil, (MInt,CDouble) :: MDouble) implements composite ${ forge_double_divide($0.toDouble,$1) }
    infix (Prim) ("/", Nil, (MFloat,CInt) :: MFloat) implements composite ${ forge_float_divide($0,$1.toFloat) }
    infix (Prim) ("/", Nil, (MFloat,CFloat) :: MFloat) implements composite ${ forge_float_divide($0,$1) }
    infix (Prim) ("/", Nil, (MFloat,CDouble) :: MDouble) implements composite ${ forge_double_divide($0.toDouble,$1) }
    infix (Prim) ("/", Nil, (MDouble,CInt) :: MDouble) implements composite ${ forge_double_divide($0,$1.toDouble) }
    infix (Prim) ("/", Nil, (MDouble,CFloat) :: MDouble) implements composite ${ forge_double_divide($0.toDouble,$1) }
    infix (Prim) ("/", Nil, (MDouble,CDouble) :: MDouble) implements composite ${ forge_double_divide($0,$1) }
    infix (Prim) ("/", Nil, (MInt,MInt) :: MInt) implements composite ${ forge_int_divide($0,$1) }
    infix (Prim) ("/", Nil, (MInt,MFloat) :: MFloat) implements composite ${ forge_float_divide($0.toFloat,$1) }
    infix (Prim) ("/", Nil, (MInt,MDouble) :: MDouble) implements composite ${ forge_double_divide($0.toDouble,$1) }
    infix (Prim) ("/", Nil, (MFloat,MInt) :: MFloat) implements composite ${ forge_float_divide($0,$1.toFloat) }
    infix (Prim) ("/", Nil, (MFloat,MFloat) :: MFloat) implements composite ${ forge_float_divide($0,$1) }
    infix (Prim) ("/", Nil, (MFloat,MDouble) :: MDouble) implements composite ${ forge_double_divide($0.toDouble,$1) }
    infix (Prim) ("/", Nil, (MDouble,MInt) :: MDouble) implements composite ${ forge_double_divide($0,$1.toDouble) }
    infix (Prim) ("/", Nil, (MDouble,MFloat) :: MDouble) implements composite ${ forge_double_divide($0,$1.toDouble) }
    infix (Prim) ("/", Nil, (MDouble,MDouble) :: MDouble) implements composite ${ forge_double_divide($0,$1) }
  }
  
  def importMisc() = {
    val Misc = grp("Misc")
    
    direct (Misc) ("exit", Nil, MInt :: MUnit, effect = simple) implements codegen($cala, ${sys.exit($0)})
    direct (Misc) ("print", Nil, MAny :: MUnit, effect = simple) implements codegen($cala, ${print($0)})
    direct (Misc) ("fatal", Nil, MString :: MNothing, effect = simple) implements codegen($cala, ${throw new Exception($0)})
    
    val println = direct (Misc) ("println", List(), List(MAny) :: MUnit, effect = simple)
    val println2 = direct (Misc) ("println", List(), List() :: MUnit, effect = simple)
    impl (println) (codegen($cala, "println(" + quotedArg(0) + ")"))
    impl (println2) (codegen($cala, "println()"))
    
    val whileDo = direct (Misc) ("__whileDo", List(), List(MThunk(MBoolean),MThunk(MUnit)) :: MUnit, effect = simple)        
    
    // function (block) arguments should be referenced using $b[<arg name>]
    impl (whileDo) (codegen($cala, ${
      while ($b[0]) {
        $b[1]
      }
    }))
    
    // TODO: something is broken with IfThenElse here; bound symbols (effects) are getting hoisted if the frequencies are not set to cold.    
    val T = tpePar("T")
    val ifThenElse = direct (Misc) ("__ifThenElse", List(T), List(MThunk(MBoolean),MThunk(T,cold),MThunk(T,cold)) :: T) 
    impl (ifThenElse) (codegen($cala, ${
      if ($b[0]) {
        $b[1]
      } 
      else {
        $b[2]
      }
    }))
    
    val immutable = infix (Misc) ("unsafeImmutable", List(T), List(T) :: T, aliasHint = copies(0))
    impl (immutable) (codegen($cala, quotedArg(0)))
  }
  
  def importCasts() = {
    val Cast = grp("Cast")
    val A = tpePar("A")    
    val B = tpePar("B")    
    
    // these don't work as infix_ methods
    noInfixList :::= List("AsInstanceOf", "IsInstanceOf") 
    
    infix (Cast) ("AsInstanceOf", (A,B), A :: B) implements codegen($cala, ${ $0.asInstanceOf[$t[B]] })
    infix (Cast) ("IsInstanceOf", (A,B), A :: MBoolean) implements codegen($cala, ${ $0.isInstanceOf[$t[B]] })
  }
  
  def importNumerics() = {
    val Num = grp("Numeric")    
    val T = tpePar("T")    
    
    lift (Num) (T withBound TNumeric)
        
    val zero = infix (Num) ("zero", List(T withBound TNumeric), List() :: T)
    val plus = infix (Num) ("+", List(T withBound TNumeric), List(T,T) :: T)    
    val minus = infix (Num) ("-", List(T withBound TNumeric), List(T,T) :: T)     
    val times = infix (Num) ("*", List(T withBound TNumeric), List(T,T) :: T)    
    impl (zero) (codegen($cala, "implicitly[Numeric["+quotedTpe(0,zero)+"]].zero"))
    impl (plus) (codegen($cala, quotedArg(0) + " + " + quotedArg(1)))
    impl (minus) (codegen($cala, quotedArg(0) + " - " + quotedArg(1)))
    impl (times) (codegen($cala, quotedArg(0) + " * " + quotedArg(1)))
    
    val Frac = grp("Fractional")    
    val R = tpePar("R")
    val div = infix (Frac) ("/", List(T,R withBound TFractional), (T,R) :: R, T ==> R)
    impl (div) (codegen($cala, ${ implicitly[Fractional[$t[R]]].div($0,$1) }))
  }
  
  def importOrdering() = {
    val Ord = grp("Ordering2") // Ordering gets pulled in by DeliteArrayOps, need to disambiguate
    val A = tpePar("A")
    val B = tpePar("B")    
    val AC = tpePar("A", stage = now)
    val BC = tpePar("B", stage = now)
    
    direct (Ord) ("__equal", (A,B), (A,B) :: MBoolean) implements (codegen($cala, quotedArg(0) + " == " + quotedArg(1)))
    direct (Ord) ("__equal", (A,BC), (A,BC) :: MBoolean) implements (codegen($cala, quotedArg(0) + " == " + quotedArg(1)))
    direct (Ord) ("__equal", (AC,B), (AC,B) :: MBoolean) implements (codegen($cala, quotedArg(0) + " == " + quotedArg(1)))
    
    val lt = infix (Ord) ("<", List(A withBound TOrdering), List(A,A) :: MBoolean)    
    val lte = infix (Ord) ("<=", List(A withBound TOrdering), List(A,A) :: MBoolean)    
    val gt = infix (Ord) (">", List(A withBound TOrdering), List(A,A) :: MBoolean)    
    val gte = infix (Ord) (">=", List(A withBound TOrdering), List(A,A) :: MBoolean)    
    
    impl (lt) (codegen($cala, quotedArg(0) + " < " + quotedArg(1)))    
    impl (lte) (codegen($cala, quotedArg(0) + " <= " + quotedArg(1)))    
    impl (gt) (codegen($cala, quotedArg(0) + " > " + quotedArg(1)))
    impl (gte) (codegen($cala, quotedArg(0) + " >= " + quotedArg(1)))

    val le = infix (Ord) ("le", List(A withBound TOrdering), List(A,A) :: MBoolean)
    val ge = infix (Ord) ("ge", List(A withBound TOrdering), List(A,A) :: MBoolean)

    impl (le) (codegen($cala, quotedArg(0) + " <= " + quotedArg(1)))
    impl (ge) (codegen($cala, quotedArg(0) + " >= " + quotedArg(1)))

    val eq = infix (Ord) ("eq", List(A withBound TOrdering), List(A,A) :: MBoolean)
    val ne = infix (Ord) ("neq", List(A withBound TOrdering), List(A,A) :: MBoolean)

    impl (eq) (codegen($cala, quotedArg(0) + " == " + quotedArg(1)))
    impl (ne) (codegen($cala, quotedArg(0) + " != " + quotedArg(1)))
  }
  
  def importStrings() = {
    val Str = grp("String")
    lift (Str) (MString)
    
    // overloaded variants of string concat
    val T = tpePar("T") 

    // most of these variants collapse to a common back-end implementation:
    val xcharAt = infix (Str) ("xcharAt", List(), List(MString, MInt) :: MString)
    impl (xcharAt) (codegen($cala, quotedArg(0)+"("+quotedArg(1)+").toString"))

    val xsplit = infix (Str) ("xsplit", List(), List(MString, MString) :: MArray(MString))
    impl (xsplit) (codegen($cala, quotedArg(0)+".split("+quotedArg(1)+")"))

    val replaceFirst = infix (Str) ("replaceFirst", List(), List(MString, MString, MString) :: MString)
    impl (replaceFirst) (codegen($cala, quotedArg(0)+".replaceFirst("+quotedArg(1)+", "+quotedArg(2)+")"))

    val replaceAllLiterally = infix (Str) ("replaceAllLiterally", List(), List(MString, MString, MString) :: MString)
    impl (replaceAllLiterally) (codegen($cala, quotedArg(0)+".replaceAllLiterally("+quotedArg(1)+", "+quotedArg(2)+")"))

    val indexOf = infix (Str) ("indexOf", List(), List(MString, MString) :: MInt)
    impl (indexOf) (codegen($cala, quotedArg(0)+".indexOf("+quotedArg(1)+")"))

    val lastIndexOf = infix (Str) ("lastIndexOf", List(), List(MString, MString) :: MInt)
    impl (lastIndexOf) (codegen($cala, quotedArg(0)+".lastIndexOf("+quotedArg(1)+")"))

    val size = infix (Str) ("size", List(), List(MString) :: MInt)
    impl (size) (codegen($cala, quotedArg(0)+".size"))

    val substring1 = infix (Str) ("substring", List(), List(MString, MInt, MInt) :: MString)
    impl (substring1) (codegen($cala, quotedArg(0)+".substring("+quotedArg(1)+", "+quotedArg(2)+")"))
    val substring2 = infix (Str) ("substring", List(), List(MString, MInt) :: MString)
    impl (substring2) (codegen($cala, quotedArg(0)+".substring("+quotedArg(1)+")"))    

    // maps to Rep[String], Rep[Any]
    val concat = infix (Str) ("+", List(T), List(CString, T) :: MString)
    val concat2 = infix (Str) ("+", List(T), List(MString, T) :: MString)
    val concat3 = infix (Str) ("+", List(T), List(CString, MVar(T)) :: MString)
    val concat4 = infix (Str) ("+", List(T), List(MString, MVar(T)) :: MString)
    
    // Rep[Any], Rep[String]
    val concat5 = infix (Str) ("+", List(T), List(T, CString) :: MString)
    val concat6 = infix (Str) ("+", List(T), List(T, MString) :: MString)
    val concat7 = infix (Str) ("+", List(T), List(MVar(T), CString) :: MString)
    val concat8 = infix (Str) ("+", List(T), List(MVar(T), MString) :: MString)
    
    // Rep[String], Rep[String]
    val concat9 = infix (Str) ("+", List(), List(MString, CString) :: MString)
    val concat10 = infix (Str) ("+", List(), List(CString, MString) :: MString)
    val concat11 = infix (Str) ("+", List(), List(MString, MString) :: MString)
    
    // TODO: we would like overloaded variants to possibly use the same codegen impl instead of being redundant here    
    // most of the concat codegens are not used, but it is not easy to tell which ones will "make it"
    // should overloading be more explicit in the spec to avoid this problem? (an 'overloaded' parameter?)
    // at the very least, we should check for inconsistent codegen rules (or impls) between overloaded variants that collapse to the same thing
    def scalaStrConcat(o: Rep[DSLOp]) = quotedArg(0)+".toString + " + quotedArg(1)+".toString"
    impl (concat) (codegen($cala, scalaStrConcat(concat)))
    impl (concat2) (codegen($cala, scalaStrConcat(concat)))
    impl (concat3) (codegen($cala, scalaStrConcat(concat)))
    impl (concat4) (codegen($cala, scalaStrConcat(concat)))
    impl (concat5) (codegen($cala, scalaStrConcat(concat)))
    impl (concat6) (codegen($cala, scalaStrConcat(concat)))
    impl (concat7) (codegen($cala, scalaStrConcat(concat)))
    impl (concat8) (codegen($cala, scalaStrConcat(concat)))
    impl (concat9) (codegen($cala, scalaStrConcat(concat)))
    impl (concat10) (codegen($cala, scalaStrConcat(concat)))
    impl (concat11) (codegen($cala, scalaStrConcat(concat)))    
  }
  
  def importMath() = {
    val Math = grp("Math")
    
    val maxInt = static (Math) ("max", List(), List(MInt,MInt) :: MInt)
    val maxDbl = static (Math) ("max", List(), List(MDouble,MDouble) :: MDouble)
    val ceil = static(Math) ("ceil", Nil, MDouble :: MDouble)
    val exp = static(Math) ("exp", Nil, MDouble :: MDouble)
    val log = static(Math) ("log", Nil, MDouble :: MDouble)
    val abs = static(Math) ("abs", Nil, MDouble :: MDouble)
    
    impl (maxInt) (codegen($cala, "scala.math.max(" + quotedArg(0) + ", " + quotedArg(1) + ")"))
    impl (maxDbl) (codegen($cala, "scala.math.max(" + quotedArg(0) + ", " + quotedArg(1) + ")"))
    impl (ceil) (codegen($cala, "java.lang.Math.ceil(" + quotedArg(0) + ")"))
    impl (exp) (codegen($cala, "java.lang.Math.exp(" + quotedArg(0) + ")"))
    impl (log) (codegen($cala, "java.lang.Math.log(" + quotedArg(0) + ")"))
    impl (abs) (codegen($cala, "java.lang.Math.abs(" + quotedArg(0) + ")"))
  }  
  
  // tuples need some work, the tricks bordering on hacks in here are pretty unsatisfying
  def importTuples() = {        
    val A = tpePar("A")
    val B = tpePar("B")
    val e = tpePar("_")
        
    // the abstract name needs to be different than the Scala name, since we don't want to shadow it. 
    val Tuple2 = tpe("Tup2", (A,B))
    val CTuple2 = tpe("Tuple2", (A,B), stage = now) 
            
    data(Tuple2, ("_1", A), ("_2", B))
    compiler (Tuple2) ("tuple2_get1", A, Tuple2(A,e) :: A) implements getter(0, "_1")
    compiler (Tuple2) ("tuple2_get2", B, Tuple2(e,B) :: B) implements getter(0, "_2")
    
    fimplicit (Tuple2) ("t2", (A,B), Tuple2(A,B) :: CTuple2(A,B)) implements composite ${ ((tuple2_get1($0), tuple2_get2($0))) }      
    fimplicit (Tuple2) ("make_tuple2", (A,B), CTuple2(A,B) :: Tuple2(A,B)) implements allocates(Tuple2, ${$0._1}, ${$0._2})
    fimplicit (Tuple2) ("chain_make_tuple2_var", (A,B), CTuple2(MVar(A),B) :: Tuple2(A,B)) implements composite ${ make_tuple2(($0._1,$0._2)) }
    fimplicit (Tuple2) ("chain_make_tuple2_var", (A,B), CTuple2(A,MVar(B)) :: Tuple2(A,B)) implements composite ${ make_tuple2(($0._1,$0._2)) }
    fimplicit (Tuple2) ("chain_make_tuple2_var", (A,B), CTuple2(MVar(A),MVar(B)) :: Tuple2(A,B)) implements composite ${ make_tuple2(($0._1,$0._2)) }
    
    // val C = tpePar("C")
    // val Tuple3 = tpe("Tup3", (A,B,C))
    // val CTuple3 = tpe("Tuple3", (A,B,C), stage = now)
            
    // val D = tpePar("D")    
    // val Tuple4 = tpe("Tup4", (A,B,C,D))
    // val CTuple4 = tpe("Tuple4", (A,B,C,D), stage = now)            
  }
}
