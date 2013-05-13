package LOWERCASE_DSL_NAME.compiler.extern

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
  def array_asimmutable[T:Manifest](__arg0: Rep[ForgeArray[T]])(implicit __imp0: SourceContext): Rep[ForgeArray[T]]
    = object_unsafe_immutable(__arg0)

  def array_map[A:Manifest,B:Manifest](a: Rep[ForgeArray[A]], f: Rep[A] => Rep[B]): Rep[ForgeArray[B]]
    = darray_map(a, f)
  def array_zipwith[A:Manifest,B:Manifest,R:Manifest](x: Rep[ForgeArray[A]], y: Rep[ForgeArray[B]], f: (Rep[A],Rep[B]) => Rep[R]): Rep[ForgeArray[R]]
    = darray_zipwith(x, y, f)
  def array_reduce[A:Manifest](x: Rep[ForgeArray[A]], f: (Rep[A],Rep[A]) => Rep[A], zero: Rep[A]): Rep[A]
    = darray_reduce(x, f, zero)
  def array_filter[A:Manifest](x: Rep[ForgeArray[A]], f: Rep[A] => Rep[Boolean]): Rep[ForgeArray[A]]
    = darray_filter(x, f)
  def array_mkstring[A:Manifest](a: Rep[ForgeArray[A]], del: Rep[String]): Rep[String]
    = darray_mkstring(a, del)
  def array_union[A:Manifest](lhs: Rep[ForgeArray[A]], rhs: Rep[ForgeArray[A]]): Rep[ForgeArray[A]]
    = darray_union(lhs, rhs)
  def array_intersect[A:Manifest](lhs: Rep[ForgeArray[A]], rhs: Rep[ForgeArray[A]]): Rep[ForgeArray[A]]
    = darray_intersect(lhs, rhs)
  def array_take[A:Manifest](lhs: Rep[ForgeArray[A]], n: Rep[Int])(implicit ctx: SourceContext): Rep[ForgeArray[A]]
    = darray_take(lhs, n)
  //def array_sort[A:Manifest](lhs: Rep[ForgeArray[A]]): Rep[ForgeArray[A]]
  //  = darray_sort(lhs)
  def array_range(st: Rep[Int], en: Rep[Int]): Rep[ForgeArray[Int]]
    = darray_range(st, en)
  def array_toseq[A:Manifest](a: Rep[ForgeArray[A]]): Rep[Seq[A]]
    = darray_toseq(a)
}
trait ScalaGenForgeArrayOps extends ScalaGenDeliteArrayOps with ScalaGenPrimitiveOps with ScalaGenObjectOps { val IR: DeliteArrayFatExp with DeliteOpsExp }
trait CudaGenForgeArrayOps extends CudaGenDeliteArrayOps with CudaGenPrimitiveOps with CudaGenObjectOps { val IR: DeliteArrayFatExp with DeliteOpsExp }
trait OpenCLGenForgeArrayOps extends OpenCLGenDeliteArrayOps with OpenCLGenPrimitiveOps with OpenCLGenObjectOps { val IR: DeliteArrayFatExp with DeliteOpsExp }
trait CGenForgeArrayOps extends CGenDeliteArrayOps with CGenPrimitiveOps with CGenObjectOps { val IR: DeliteArrayFatExp with DeliteOpsExp }
