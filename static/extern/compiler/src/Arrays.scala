package LOWERCASE_DSL_NAME.compiler

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._
import ppl.delite.framework.codegen.delite.overrides._
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.datastructures._

// For compiler (Delite) implementation
trait ForgeArrayOpsExp extends DeliteArrayFatExp {
  this: DeliteOpsExp =>
  
  type ForgeArray[T] = DeliteArray[T]
  implicit def forgeArrayManifest[T:Manifest] = manifest[DeliteArray[T]]
    
  def array_empty[T:Manifest](__arg0: Rep[Int])(implicit __imp0: SourceContext): Rep[ForgeArray[T]] 
    = darray_new[T](__arg0)
  def array_copy[T:Manifest](__arg0: Rep[ForgeArray[T]],__arg1: Rep[Int],__arg2: Rep[ForgeArray[T]],__arg3: Rep[Int],__arg4: Rep[Int])(implicit __imp0: SourceContext): Rep[Unit]
    = darray_copy(__arg0,__arg1,__arg2,__arg3,__arg4)
  def array_update[T:Manifest](__arg0: Rep[ForgeArray[T]],__arg1: Rep[Int],__arg2: Rep[T])(implicit __imp0: SourceContext): Rep[Unit]
    = darray_update(__arg0,__arg1,__arg2)
  def array_apply[T:Manifest](__arg0: Rep[ForgeArray[T]],__arg1: Rep[Int])(implicit __imp0: SourceContext): Rep[T]
    = darray_apply(__arg0,__arg1)
  def array_length[T:Manifest](__arg0: Rep[ForgeArray[T]])(implicit __imp0: SourceContext): Rep[Int]          
    = darray_length(__arg0)
//<<<<<<< HEAD
  def array_map[A:Manifest,B:Manifest](a: Rep[ForgeArray[A]], f: Rep[A] => Rep[B])(implicit __imp0: SourceContext): Rep[ForgeArray[B]]
    = darray_map(a, f)
  // unnecessary constraint that f creates a ForgeArray?
  def array_flatmap[A:Manifest,B:Manifest](a: Rep[ForgeArray[A]], f: Rep[A] => Rep[ForgeArray[B]])(implicit __imp0: SourceContext): Rep[ForgeArray[B]]
    = array_reduce[ForgeArray[B]](array_map[A,ForgeArray[B]](a, f), array_union[B], array_empty[B](unit(0)))
  def array_zipwith[A:Manifest,B:Manifest,R:Manifest](x: Rep[ForgeArray[A]], y: Rep[ForgeArray[B]], f: (Rep[A],Rep[B]) => Rep[R])(implicit __imp0: SourceContext): Rep[ForgeArray[R]]
    = darray_zipwith(x, y, f)
  def array_reduce[A:Manifest](x: Rep[ForgeArray[A]], f: (Rep[A],Rep[A]) => Rep[A], zero: Rep[A])(implicit __imp0: SourceContext): Rep[A]
    = darray_reduce(x, f, zero)
  def array_filter[A:Manifest](x: Rep[ForgeArray[A]], f: Rep[A] => Rep[Boolean])(implicit __imp0: SourceContext): Rep[ForgeArray[A]]
    = darray_filter(x, f)
  def array_mkstring[A:Manifest](a: Rep[ForgeArray[A]], del: Rep[String])(implicit __imp0: SourceContext): Rep[String]
    = darray_mkstring(a, del)
  def array_union[A:Manifest](lhs: Rep[ForgeArray[A]], rhs: Rep[ForgeArray[A]])(implicit __imp0: SourceContext): Rep[ForgeArray[A]]
    = darray_union(lhs, rhs)
  def array_intersect[A:Manifest](lhs: Rep[ForgeArray[A]], rhs: Rep[ForgeArray[A]])(implicit __imp0: SourceContext): Rep[ForgeArray[A]]
    = darray_intersect(lhs, rhs)
  def array_take[A:Manifest](lhs: Rep[ForgeArray[A]], n: Rep[Int])(implicit __imp0: SourceContext): Rep[ForgeArray[A]]
    = darray_take(lhs, n)
  //def array_sort[A:Manifest](lhs: Rep[ForgeArray[A]])(implicit __imp0: SourceContext): Rep[ForgeArray[A]]
  //  = darray_sort(lhs)
  def array_range(st: Rep[Int], en: Rep[Int])(implicit __imp0: SourceContext): Rep[ForgeArray[Int]]
    = darray_range(st, en)
  def array_toseq[A:Manifest](a: Rep[ForgeArray[A]])(implicit __imp0: SourceContext): Rep[Seq[A]]
    = darray_toseq(a)
      
//=======
  def array_clone[T:Manifest](__arg0: Rep[ForgeArray[T]])(implicit __imp0: SourceContext): Rep[ForgeArray[T]] = {
    val out = darray_new[T](darray_length(__arg0))
    darray_copy(__arg0, unit(0), out, unit(0), darray_length(__arg0))
    out.unsafeImmutable
  }
  def array_sort[T:Manifest:Ordering](__arg0: Rep[ForgeArray[T]])(implicit __imp0: SourceContext): Rep[ForgeArray[T]]
    = darray_sort(__arg0)
  def array_fromseq[T:Manifest](__arg0: Seq[Rep[T]])(implicit __imp0: SourceContext): Rep[ForgeArray[T]] = {
    val out = darray_new[T](unit(__arg0.length))
    for (i <- 0 until __arg0.length) {
      out(unit(i)) = __arg0(i)
    }
    out.unsafeImmutable
  }
        
//>>>>>>> master
  // avoid mixing in LMS Array ops due to conflicts. alternatively, we could refactor LMS array ops to 
  // put ArrayApply and ArrayLength in an isolated trait that we can use.
  case class ArrayApply[T:Manifest](a: Exp[Array[T]], n: Exp[Int]) extends Def[T]
  case class ArrayLength[T:Manifest](a: Exp[Array[T]]) extends Def[Int]
  
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case ArrayApply(a,x) => scala_array_apply(f(a),f(x))(mtype(manifest[A]),pos)
    case ArrayLength(a) => scala_array_length(f(a))(mtype(manifest[A]),pos)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]] // why??
  
  def scala_array_apply[T:Manifest](__arg0: Rep[Array[T]],__arg1: Rep[Int])(implicit __imp0: SourceContext): Rep[T] 
    = ArrayApply(__arg0,__arg1)
  def scala_array_length[T:Manifest](__arg0: Rep[Array[T]])(implicit __imp0: SourceContext): Rep[Int] 
    = ArrayLength(__arg0)
}
trait ScalaGenForgeArrayOps extends ScalaGenDeliteArrayOps with ScalaGenPrimitiveOps with ScalaGenObjectOps { 
  val IR: ForgeArrayOpsExp with DeliteOpsExp 
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ArrayApply(x,n) => emitValDef(sym, "" + quote(x) + "(" + quote(n) + ")")
    case ArrayLength(x) => emitValDef(sym, "" + quote(x) + ".length")
    case _ => super.emitNode(sym,rhs)
  }
}
trait CLikeGenForgeArrayOps extends CLikeGenBase {  
  val IR: ForgeArrayOpsExp with DeliteOpsExp 
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ArrayApply(x,n) => emitValDef(sym, quote(x) + ".apply(" + quote(n) + ")")
    case ArrayLength(x) => emitValDef(sym, quote(x) + ".length")
    case _ => super.emitNode(sym, rhs)
  }  
}
trait CudaGenForgeArrayOps extends CudaGenDeliteArrayOps with CLikeGenForgeArrayOps with CudaGenObjectOps { val IR: ForgeArrayOpsExp with DeliteOpsExp }
trait OpenCLGenForgeArrayOps extends OpenCLGenDeliteArrayOps with CLikeGenForgeArrayOps with OpenCLGenObjectOps { val IR: ForgeArrayOpsExp with DeliteOpsExp }
trait CGenForgeArrayOps extends CGenDeliteArrayOps with CGenObjectOps { 
  val IR: ForgeArrayOpsExp with DeliteOpsExp 
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ArrayApply(x,n) => emitValDef(sym, quote(x) + "->apply(" + quote(n) + ")")
    case ArrayLength(x) => emitValDef(sym, quote(x) + "->length")
    case _ => super.emitNode(sym, rhs)
  }
}
